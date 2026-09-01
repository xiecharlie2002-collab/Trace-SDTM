# TraceSDTM

TraceSDTM 是一个人工监督、规格驱动、可追溯的 SDTM 自动化作品集项目。它覆盖 DM、AE、VS 三个域，重点不是宣称“全自动替代程序员”，而是展示如何安全地把大模型建议接入受控的临床数据标准化流程。

## 已实现流程

```text
模拟原始数据
    ↓
字段画像与数据字典
    ↓
受约束的候选映射
    ↓
Excel 人工审核
    ↓
已批准 YAML 规格
    ↓
R 与 sdtm.oak 确定性构建
    ↓
CSV、XPT、字段追溯
    ↓
本地检查 + Pinnacle 21 Community
    ↓
离线 HTML 项目报告
```

## 当前验证结果

- 生成 DM 6 行、AE 8 行、VS 60 行。
- 原始数据画像包含 43 个字段，候选映射包含 40 个需审核任务。
- VS 覆盖身高、体重、体温、收缩压、舒张压和脉搏 6 类测量。
- 本地检查为 0 个问题。
- Pinnacle 21 Community 4.2.0.5013 使用 FDA 2508.1、SDTMIG 3.4 和 2026-03-27 受控术语完成真实验证。
- 当前 Pinnacle 21 问题均被归为明确的最小范围限制；DM、AE、VS 内没有未解释的 Reject，也没有 `generated_domain_defect`。

Pinnacle 21 仍会报告缺少 Define-XML、TS、其他未覆盖域以及部分 Expected 变量。这些原始结果全部保留，不能把本项目描述成“完整提交包通过验证”。

## 快速运行

在本目录执行：

```powershell
Rscript scripts/trace_sdtm.R recommend --seed
Rscript scripts/trace_sdtm.R approve
Rscript scripts/trace_sdtm.R run
```

`recommend --seed` 使用专家模板生成离线参考种子。它只用于演示审核、构建和评价程序，不是实际模型输出。

单独执行各阶段：

```powershell
Rscript scripts/trace_sdtm.R profile
Rscript scripts/trace_sdtm.R build
Rscript scripts/trace_sdtm.R validate-local
Rscript scripts/trace_sdtm.R doctor-p21
Rscript scripts/trace_sdtm.R validate-p21
Rscript scripts/trace_sdtm.R report
Rscript scripts/trace_sdtm.R test
```

## 使用真实大模型

配置 OpenAI 兼容接口：

```powershell
$env:TRACE_SDTM_API_KEY = '<密钥>'
$env:TRACE_SDTM_BASE_URL = 'https://api.example.com/v1'
$env:TRACE_SDTM_MODEL = '<模型名称>'
Rscript scripts/trace_sdtm.R recommend
```

真实推荐仍需打开 `output/review/mapping_review.xlsx` 完成人工决策，再运行：

```powershell
$env:TRACE_SDTM_REVIEWER = '<审核者标识>'
Rscript scripts/trace_sdtm.R approve
```

模型只能选择允许的域、变量、映射类型和已登记转换函数。任何未知映射标识、未知目标变量、非法 JSON 或任意代码执行请求都会被拒绝。

## Pinnacle 21 配置

默认配置位于 `config/p21.yml`：

```text
Community：4.2.0.5013
安装目录：D:\Pinnacle
命令行组件：1.0.9
规则引擎：FDA 2508.1
标准：SDTMIG 3.4
受控术语：2026-03-27
```

程序只调用已安装组件，不复制、修改或提交 Pinnacle 21 文件。自动调用失败时，也可以在图形界面完成验证后导入报告：

```powershell
Rscript scripts/trace_sdtm.R import-p21 --file '<报告路径>'
```

## 依赖复现

首次使用先安装 `renv`，再恢复锁定依赖：

```powershell
Rscript -e "install.packages('renv', repos='https://cloud.r-project.org')"
Rscript -e "renv::restore()"
```

项目使用 sdtm.oak 0.2.0 的日期、研究日和序号算法。依赖版本记录在 `renv.lock`。

## 重要产物

- `output/review/mapping_review.xlsx`：人工审核界面
- `specs/approved_mapping.yml`：唯一允许进入构建的映射规格
- `output/sdtm/xpt`：Pinnacle 21 实际验证的 XPT
- `output/lineage/field_lineage.csv`：字段级追溯
- `output/validation/p21/p21_report.xlsx`：原始验证报告
- `output/report/trace_sdtm_report.html`：离线项目报告

## 边界

本项目不包含 Define-XML、aCRF、完整试验设计域、ADaM、TLF、电子签名、多用户权限、法规申报级验证或 CDISC CORE。所有输出仍需合格的临床数据标准专家审核。

五分钟演示说明见 `docs/demo.md`。

