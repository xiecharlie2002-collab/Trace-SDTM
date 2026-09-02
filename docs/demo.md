# TraceSDTM 0.2 五分钟演示

## 0:00—0:40：项目定位

“TraceSDTM 不是把原始数据交给模型后直接收代码。模型分两阶段提出类别和函数链；注册表限制函数、参数、来源和目标；人工审核完整方案；正式构建只读取批准的 YAML。”

## 0:40—1:25：注册表和两阶段推荐

```powershell
Rscript scripts/trace_sdtm.R registry-check --scenario advanced
Rscript scripts/trace_sdtm.R recommend --seed --scenario advanced
```

打开 `output/scenarios/advanced/review/mapping_review.xlsx`：

- Concept Review 以临床概念为审核单位；
- Candidate Plans 和 Plan Steps 展示完整候选方案；
- Transform Catalog 由注册表自动生成；
- 当前使用专家参考种子，不将它宣传为模型准确率。

## 1:25—2:00：锁定规格

```powershell
Rscript scripts/trace_sdtm.R approve --scenario advanced
```

打开 `output/scenarios/advanced/specs/approved_mapping.yml`。说明 approve 和 build 都会重新进行注册表检查，构建期间不调用模型；规格保存审核者、时间、注册表版本和校验值。

## 2:00—3:10：高级构建与追溯

```powershell
Rscript scripts/trace_sdtm.R build --scenario advanced
```

依次展示：

- 两个暴露文件取得最早给药日期时间；
- AE 与 SAE 受控一对一连接；
- 不完整日期保持最大已知精度且不填补；
- 英寸、磅、华氏度通过版本化换算表转换；
- VS 横向转纵向并派生 VSBLFL；
- `field_lineage.csv` 记录每一步、连接规则、sdtm.oak 版本、审核者和记录数。

## 3:10—4:10：双层验证

```powershell
Rscript scripts/trace_sdtm.R validate-local --scenario advanced
Rscript scripts/trace_sdtm.R validate-p21 --scenario advanced
```

- 本地检查 0 个问题；
- Pinnacle 21 的 DM、AE、VS 域内问题为 0；
- 缺少 Define-XML 和 TS 的 Reject 原样保留并归为最小范围。

## 4:10—5:00：报告和结论

```powershell
Rscript scripts/trace_sdtm.R report --scenario advanced
```

打开 `output/scenarios/advanced/report/trace_sdtm_report.html`。

“项目体现的不是提示词技巧，而是可治理的自动化设计：模型推荐、专家判断、注册表约束、确定性执行、独立验证和全程追溯。高级场景专门证明它能处理真实项目中较难的连接、日期精度和单位问题。”

## 简历表述示例

基于 R、sdtm.oak 与 Pinnacle 21 Community 构建人工监督的 SDTM 自动化流水线，覆盖 DM、AE、VS，实现临床概念分组、两阶段映射推荐、转换注册表约束、Excel 审核、YAML 规格锁定、确定性 XPT 生成及字段级追溯；设计跨来源连接、不完整日期、受控单位换算和 VS 基线标志场景，并将验证问题区分为生成缺陷与最小范围限制。
