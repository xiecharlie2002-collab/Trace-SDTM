[CmdletBinding()]
param(
    [ValidateRange(1, 65535)]
    [int]$Port,
    [switch]$NoBrowser,
    [switch]$Restore
)

$ErrorActionPreference = 'Stop'
$projectRoot = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '..')).Path
$entryPoint = Join-Path $projectRoot 'scripts\trace_sdtm.R'
$rscript = (Get-Command Rscript.exe -ErrorAction Stop).Source

if ($Restore) {
    Write-Host '正在恢复 renv 依赖……'
    & $rscript -e "renv::restore(project = '$($projectRoot.Replace('\', '/'))', prompt = FALSE)"
    if ($LASTEXITCODE -ne 0) { throw 'renv 依赖恢复失败。' }
}

function Test-LoopbackPortAvailable {
    param([int]$Candidate)
    $listener = $null
    try {
        $listener = [System.Net.Sockets.TcpListener]::new([System.Net.IPAddress]::Loopback, $Candidate)
        $listener.Start()
        return $true
    }
    catch {
        return $false
    }
    finally {
        if ($null -ne $listener) { $listener.Stop() }
    }
}

if ($PSBoundParameters.ContainsKey('Port')) {
    if (-not (Test-LoopbackPortAvailable -Candidate $Port)) {
        throw "指定端口 $Port 已被占用。"
    }
    $selectedPort = $Port
}
else {
    $selectedPort = 3838..3848 | Where-Object { Test-LoopbackPortAvailable -Candidate $_ } | Select-Object -First 1
    if ($null -eq $selectedPort) { throw '3838至3848端口均被占用。' }
}

$env:TRACE_SDTM_ROOT = $projectRoot
$arguments = @($entryPoint, 'studio', '--port', [string]$selectedPort)
if ($NoBrowser) { $arguments += '--no-browser' }

Write-Host "TraceSDTM Studio 将仅监听 http://127.0.0.1:$selectedPort"
Write-Host '按 Ctrl+C 停止工作台。'
& $rscript @arguments
exit $LASTEXITCODE
