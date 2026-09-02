# TraceSDTM

TraceSDTM 是一个人工监督、注册表约束、规格驱动、可追溯的 SDTM 自动化作品集项目。0.2 版继续只覆盖 DM、AE、VS，但增加了多来源整合、不完整日期时间、受控单位换算、VS 横向转纵向和基线标志。

## 流程

```text
来源数据登记与画像
    ↓
按域、表单、临床概念和依赖关系分组
    ↓
模型第一阶段：判断转换类别
    ↓
模型第二阶段：在类别内选择登记函数和参数
    ↓
按完整映射方案进行人工审核
    ↓
注册表复核并锁定 0.2 YAML 规格
    ↓
R 与 sdtm.oak 确定性生成 CSV、XPT 和追溯
    ↓
本地检查 + Pinnacle 21 Community
    ↓
评价明细与离线 HTML 报告
```

模型不能生成或执行 R 代码、单位公式、正则表达式、连接键或自由条件。正式构建只读取批准后的 YAML，不再次调用模型。

## 两个演示场景

`basic` 用于快速展示既有 DM、AE、VS 流程；`advanced` 独立展示 0.2 新能力，不覆盖基础场景和 0.1 历史成果。

高级场景当前实际结果：

- 画像包含 57 个来源字段，形成 21 个临床概念审核任务。
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

## 使用真实大模型

配置 OpenAI 兼容接口后运行：

```powershell
$env:TRACE_SDTM_API_KEY = '<密钥>'
$env:TRACE_SDTM_BASE_URL = 'https://api.example.com/v1'
$env:TRACE_SDTM_MODEL = '<模型名称>'
$env:TRACE_SDTM_EXPERIMENT_ID = 'model-20260902'
Rscript scripts/trace_sdtm.R recommend --scenario advanced
```

实验产物写入 `output/v0.2/advanced/experiments/<实验编号>/`，不会覆盖基线。打开该目录中的 `review/mapping_review.xlsx` 完成人工审核后运行：

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

`config/transform_registry.yml` 是 23 个转换函数的唯一元数据源，同时服务于提示词、模型结果验证、审核工作簿、YAML 检查、函数调度、自动测试和文档生成。

项目选择性使用 sdtm.oak 0.2.0 的普通赋值、受控术语、日期时间、参考日期、序号、研究日和基线算法。单位换算由项目版本化受控表完成。参数使用 JSON Schema Draft-07 验证，函数只能从 R 中的受控绑定表解析。

自动生成的函数目录见 `docs/transform_catalog.md`，整体架构见 `docs/architecture.md`。

## Pinnacle 21

默认本机配置位于 `config/p21.yml`：

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

## 依赖复现

```powershell
Rscript -e "install.packages('renv', repos='https://cloud.r-project.org')"
Rscript -e "renv::restore()"
```

## 重要产物

- `config/transform_registry.yml`：唯一转换元数据源。
- `output/scenarios/advanced/review/mapping_review.xlsx`：按临床概念审核的工作簿。
- `output/scenarios/advanced/specs/approved_mapping.yml`：唯一允许进入构建的 0.2 规格。
- `output/scenarios/advanced/sdtm/xpt`：Pinnacle 21 实际验证的 XPT。
- `output/scenarios/advanced/lineage/field_lineage.csv`：字段级追溯。
- `output/scenarios/advanced/validation/p21/p21_report.xlsx`：原始验证报告。
- `output/scenarios/advanced/report/trace_sdtm_report.html`：可离线打开的项目报告。

## 边界

本项目不包含 Define-XML、aCRF、完整试验设计域、ADaM、TLF、电子签名、多用户权限、法规申报级系统验证或 CDISC CORE。所有输出仍需合格的临床数据标准专家审核。

五分钟演示见 `docs/demo.md`。0.1 阶段的真实 DeepSeek 盲评记录保留在 `docs/deepseek_v4_flash_blind_experiment.md`。
