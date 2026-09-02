# TraceSDTM 上下文修复与子代理对照实验

这是单次配对实验，不能据此估计模型结果的长期稳定性。

- Codex 第一阶段有效分类：21/21
- Codex 第二阶段有效候选：13/21
- Codex 完整首选方案正确：5/21
- 通过严格第二阶段校验后的专家直接接受：5/13
- 通过严格第二阶段校验后的修改后接受：8/13
- DeepSeek：等待已轮换的新接口密钥，当前未运行。

完整指标见 `paired_metrics.csv`，错误分类见 `error_taxonomy.csv`，第一阶段配对校验值见 `stage1_prompt_hashes.csv`。
