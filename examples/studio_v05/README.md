# TraceSDTM Studio 0.5 脱敏示例

本目录保存一次高级模板的固定响应集成验收结果，用于在没有模型密钥时查看工作台最终产物。输入数据来自仓库内的公开模拟数据。

这不是模型准确率实验，也不表示法规申报包完整通过。示例只覆盖 DM、AE、VS；Pinnacle 21 报告中因未生成 Define-XML、TS 和其他域而出现的范围问题被完整保留。

关键结果：

- 确定性生成 DM 4 行、AE 5 行、VS 44 行；
- 本地高严重程度问题为 0；
- Pinnacle 21 Community 4.2.0 已真实运行并导入 31 条明细问题；
- 原始数据、字段示例值和接口密钥未包含在证据目录中；
- [离线报告](report/trace_sdtm_report.html) 可直接打开。

`recommendations/model_run.json` 明确标记为 `sanitized_acceptance_fixture`。它只证明工作台的上传后流程、审核、构建、验证、追溯、报告和证据导出可以复现。
