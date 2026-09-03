# TraceSDTM 0.4 五分钟演示

## 0:00—0:40：项目定位

“TraceSDTM 不是把原始数据交给模型后直接收代码。模型依次识别目标、选择函数、补全少量未知参数；已知参数由程序注入；注册表限制函数、参数、来源和目标；正式构建只读取批准的 YAML。”

## 0:40—1:25：注册表和三阶段推荐

```powershell
Rscript scripts/trace_sdtm.R registry-check --scenario advanced
Rscript scripts/trace_sdtm.R recommend --seed --scenario advanced
Rscript scripts/trace_sdtm.R evaluate-v04 --scenario advanced
```

打开 `output/benchmark/v2/advanced/review/mapping_review.xlsx`：

- Task Review 以原子临床动作为审核单位；
- Candidate Plans 和 Plan Steps 展示完整候选方案；
- Transform Catalog 由注册表自动生成；
- 当前使用专家参考种子，不将它宣传为模型准确率。

## 1:25—2:00：锁定规格

```powershell
Rscript scripts/trace_sdtm.R approve --scenario advanced
```

打开 `output/benchmark/v2/advanced/specs/approved_mapping.yml`。说明 approve 和 build 都会重新进行注册表检查，构建期间不调用模型；规格保存审核者、时间、注册表版本和校验值。

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

打开 `output/benchmark/v2/advanced/report/trace_sdtm_report.html`。

“项目体现的不是提示词技巧，而是可治理的自动化设计：模型推荐、专家判断、注册表约束、确定性执行、独立验证和全程追溯。高级场景专门证明它能处理真实项目中较难的连接、日期精度和单位问题。”

## 简历表述示例

基于 R、sdtm.oak 与 Pinnacle 21 Community 构建人工监督的 SDTM 自动化流水线，覆盖 DM、AE、VS；将124个映射动作拆为目标识别、函数选择和有限参数补全，利用注册表与项目政策确定性注入已知参数，实现 Excel 审核、0.4 YAML 规格锁定、XPT 生成及字段级追溯；以三级盲评分别报告语义、结构和完整计划正确性。
