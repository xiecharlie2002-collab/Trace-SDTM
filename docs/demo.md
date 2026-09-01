# TraceSDTM 五分钟演示

## 演示目标

用一个最小但完整的流程说明：大模型只负责给出受约束的候选映射，人工审核后形成固定规格，R 程序根据规格稳定生成 SDTM，最后由本地规则和 Pinnacle 21 Community 独立检查。

## 演示步骤

1. 打开 `output/profile/source_dictionary.csv`，展示系统自动提取的字段类型、示例值、缺失比例和唯一值数量。
2. 打开 `output/review/mapping_review.xlsx`，解释接受、修改、拒绝和信息不足四种审核状态。
3. 打开 `specs/approved_mapping.yml`，说明构建只读取已批准规格，不执行模型生成的任意代码。
4. 展示 `output/sdtm/csv/vs.csv`，说明 6 类横向生命体征如何转为 60 条纵向记录。
5. 打开 `output/lineage/field_lineage.csv`，从目标变量追溯到来源字段、转换函数和审核者。
6. 打开 `output/report/trace_sdtm_report.html`，展示本地检查为零、Pinnacle 21 的真实报告及范围分类。

## 必须主动说明

- 当前仓库中的候选映射是 `reference_seed`，不是一次真实接口调用的结果。
- 配置大模型接口后，执行 `recommend` 才会产生可用于模型评价的结果。
- Pinnacle 21 的 Reject 来自缺少 Define-XML 和 TS，不表示三个已生成域存在 Reject。
- 该项目是作品集原型，不是法规申报级系统。

## 简历表述示例

基于 R、sdtm.oak 与 Pinnacle 21 Community 构建人工监督的 SDTM 自动化流水线，覆盖 DM、AE、VS 三个域，实现受约束映射推荐、Excel 审核、YAML 规格锁定、确定性 XPT 生成、字段级追溯及双层验证；将 6 类生命体征由横向结构转换为 60 条纵向记录，并对验证问题按生成缺陷与最小范围限制进行可审计分类。

