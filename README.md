# TraceSDTM

TraceSDTM 是一个人工监督、注册表约束、规格驱动、可追溯的 SDTM 自动化作品集项目。0.4 版继续只覆盖 DM、AE、VS，但把模型推荐拆成目标识别、函数选择和有限参数补全，并把已知参数交给程序确定性注入。

## 流程

```text
来源数据登记与画像
    ↓
拆成带依赖关系的原子临床动作
    ↓
模型第一阶段：识别目标变量
    ↓
模型第二阶段：选择登记函数和来源编号
    ↓
政策、注册表和资源目录自动注入已知参数
    ↓
模型第三阶段：只补全仍未知且有有限选项的参数
    ↓
确定性组装、人工审核并锁定 0.4 YAML 规格
    ↓
R 与 sdtm.oak 确定性生成 CSV、XPT 和追溯
    ↓
本地检查 + Pinnacle 21 Community
    ↓
评价明细与离线 HTML 报告
```

模型不能生成或执行 R 代码、单位公式、正则表达式、连接键或自由条件。正式构建只读取批准后的 YAML，不再次调用模型。

## 三级基准场景

`basic` 包含18个常见原子任务，`intermediate` 包含49个同一数据集内的关系型任务，`advanced` 包含57个跨来源、不完整日期、单位换算和多阶段依赖任务。合计124项，并可聚合回原来的18、20和21个审核概念。

一次按层级和域隔离的真实子代理盲评中，语义正确和端到端结构正确均为124/124；真实串联完整计划为121/124，给定正确上游决定的条件完整计划为122/124。该结果来自单次、按域聚集的实验，只用于说明流程的可评价性，不用于估计模型长期正确率。详细分层结果、残余错误和协议边界见 `docs/v04_three_stage_benchmark.md`。

高级场景当前实际结果：

- 画像包含57个来源字段，形成57个原子任务，并聚合为21个审核概念。
- 生成 DM 4 行、AE 5 行、VS 44 行。
- 从两个暴露来源取得各受试者最早给药日期时间，生成 RFSTDTC。
- AE 保留年、年月和完整日期时间等不同精度，不进行日期填补。
- `70 in → 177.8 cm`、`180 lb → 81.6 kg`、`98.6 F → 37.0 C` 已通过自动测试。SDTM 中将 `lb` 规范为受控术语 `LB`，原始来源仍保留在追溯证据中。
- 24 条给药前最后一次有效检查记录被标记为 VSBLFL。
- 本地检查为 0 个问题。
- Pinnacle 21 Community 4.2.0.5013 已用 FDA 2508.1、SDTMIG 3.4 和 2026-03-27 受控术语真实验证；DM、AE、VS 没有域内缺陷，只保留缺少 Define-XML、TS 等最小范围问题。

项目不生成完整提交包，因此不能把范围问题描述为“全部合规通过”。

## 快速运行

```powershell
Rscript scripts/trace_sdtm.R registry-check --scenario advanced
Rscript scripts/trace_sdtm.R recommend --seed --scenario advanced
Rscript scripts/trace_sdtm.R evaluate-v04 --scenario advanced
Rscript scripts/trace_sdtm.R approve --scenario advanced
Rscript scripts/trace_sdtm.R run --scenario advanced
```

`recommend --seed` 使用专家金标准生成离线参考种子，只用于演示审核、构建和评价程序，不是实际模型输出。

常用命令：

```powershell
Rscript scripts/trace_sdtm.R registry-docs --scenario advanced
Rscript scripts/trace_sdtm.R profile --scenario advanced
Rscript scripts/trace_sdtm.R build --scenario advanced
Rscript scripts/trace_sdtm.R validate-local --scenario advanced
Rscript scripts/trace_sdtm.R doctor-p21 --scenario advanced
Rscript scripts/trace_sdtm.R validate-p21 --scenario advanced
Rscript scripts/trace_sdtm.R report --scenario advanced
Rscript scripts/trace_sdtm.R test --scenario advanced
```

三级子代理盲评使用一次性编排脚本，原始响应按字节冻结，正式口径不进行结构修复或选择性重试：

```powershell
Rscript scripts/experiments/v04_subagent_benchmark.R prepare-targets --scenario basic --domain DM --experiment-id <实验编号>
Rscript scripts/experiments/v04_subagent_benchmark.R import-targets --scenario basic --domain DM --experiment-id <实验编号> --response-file <响应文件> --task-id <子代理任务编号>
Rscript scripts/experiments/v04_report.R --experiment-id <实验编号>
```

完整协议还包括函数阶段、给定正确目标的条件函数阶段、参数阶段和场景汇总。每个层级与域使用一个全新子代理，同一子代理完成该域的各阶段；子代理盲法属于过程约束，不是操作系统级隔离。

## 使用真实大模型

配置 OpenAI 兼容接口后运行：

```powershell
$env:TRACE_SDTM_API_KEY = '<密钥>'
$env:TRACE_SDTM_BASE_URL = 'https://api.example.com/v1'
$env:TRACE_SDTM_MODEL = '<模型名称>'
$env:TRACE_SDTM_EXPERIMENT_ID = 'model-20260902'
Rscript scripts/trace_sdtm.R recommend --scenario advanced
```

接口密钥只应在当前终端会话中设置，不得写入源码、配置、日志或报告。如果确实需要本地持久化，可以复制 `.Renviron.example` 为 `.Renviron`；本地文件已被 Git 忽略。任何曾出现在聊天记录、终端记录或提交中的密钥都应立即撤销。完整要求见 [安全说明](SECURITY.md)。

常规模型实验产物写入 `output/benchmark/v2/<场景>/experiments/<实验编号>/`，不会覆盖基线。`recommend-targets`、`recommend-functions`、`recommend-parameters` 和 `assemble-recommendations` 也可分别运行，便于恢复和定位错误。打开实验目录中的 `review/mapping_review.xlsx` 完成人工审核后运行：

```powershell
$env:TRACE_SDTM_REVIEWER = '<审核者标识>'
Rscript scripts/trace_sdtm.R approve --scenario advanced
Rscript scripts/trace_sdtm.R run --scenario advanced
```

为了可重复评价，可以用专家金标准代填审核表：

```powershell
Rscript scripts/trace_sdtm.R review-gold --scenario advanced
Rscript scripts/trace_sdtm.R approve --scenario advanced
```

这只适用于作品集实验，不等同于法规流程中的独立专家签字。

## 注册表和 sdtm.oak

`config/transform_registry.yml` 1.2.0 是转换函数及参数解析策略的唯一元数据源，同时服务于提示词、模型结果验证、审核工作簿、YAML 检查、函数调度、自动测试和文档生成。

项目选择性使用 sdtm.oak 0.2.0 的普通赋值、受控术语、日期时间、参考日期、序号、研究日和基线算法。单位换算由项目版本化受控表完成。参数使用 JSON Schema Draft-07 验证，函数只能从 R 中的受控绑定表解析。

自动生成的函数目录见 `docs/transform_catalog.md`，整体架构见 `docs/architecture.md`。

## Pinnacle 21

共享配置位于 `config/p21.yml`，只保存版本和验证规则。本机安装路径使用不提交的 `config/p21.local.yml`：

```powershell
Copy-Item config/p21.local.example.yml config/p21.local.yml
```

复制后修改其中四个路径。本项目完成验证时使用的本机环境为：

```text
Community：4.2.0.5013
安装目录：D:\Pinnacle
命令行组件：1.0.9
规则引擎：FDA 2508.1
标准：SDTMIG 3.4
受控术语：2026-03-27
```

程序只调用已安装组件，不复制、修改或提交 Pinnacle 21 文件。也可以导入图形界面生成的报告：

```powershell
Rscript scripts/trace_sdtm.R import-p21 --file '<报告路径>' --scenario advanced
```

还可以使用 `TRACE_SDTM_P21_CONFIG` 指定其他本地配置文件，或分别设置 `TRACE_SDTM_P21_EXECUTABLE`、`TRACE_SDTM_P21_JAVA`、`TRACE_SDTM_P21_CLIENT_JAR` 和 `TRACE_SDTM_P21_CONFIG_ROOT`。未安装 Pinnacle 21 时，外部环境测试会明确跳过，不影响本地转换和规则测试。

## 依赖复现

```powershell
Rscript -e "install.packages('renv', repos='https://cloud.r-project.org')"
Rscript -e "renv::restore()"
```

## 重要产物

- `config/transform_registry.yml`：唯一转换元数据源。
- `specs/benchmark/v2/*_tasks.yml`：124项原子任务及稳定来源编号。
- `specs/benchmark/v2/*_gold.yml`：只用于盲评后的独立评价。
- `output/benchmark/v2/<场景>/review/mapping_review.xlsx`：按原子任务审核的工作簿。
- `output/benchmark/v2/<场景>/specs/approved_mapping.yml`：唯一允许进入构建的0.4规格。
- `output/benchmark/v2/<场景>/sdtm/xpt`：供 Pinnacle 21 验证的 XPT。
- `output/benchmark/v2/<场景>/lineage/field_lineage.csv`：字段级追溯。

`output/` 默认忽略新生成文件，避免先导实验、日志和重复输出被批量提交。当前 Git 标签已经保存正式基线；以后若要保存新的正式报告或冻结实验，应逐项复核后使用 `git add -f <明确路径>`，不要执行 `git add .`。

当前 `main` 只保留 v0.4 的正式盲评证据、对应离线报告和最新高级场景构建验证结果。较早版本的结果不再出现在 `main`，需要复核时可通过 v0.1—v0.4 的 Git 标签查看。

## 边界

本项目不包含 Define-XML、aCRF、完整试验设计域、ADaM、TLF、电子签名、多用户权限、法规申报级系统验证或 CDISC CORE。所有输出仍需合格的临床数据标准专家审核。

仓库中的数据均为公开示例数据的改编版本或模拟数据，不对应真实受试者。数据使用边界见 [数据来源说明](DATA_SOURCES.md)，软件许可见 [MIT许可证](LICENSE)。

五分钟演示见 `docs/demo.md`，v0.4 评价设计见 `docs/v04_three_stage_benchmark.md`。0.1 阶段的真实 DeepSeek 盲评记录保留在 `docs/deepseek_v4_flash_blind_experiment.md`。
