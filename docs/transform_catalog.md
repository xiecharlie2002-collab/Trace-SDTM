# TraceSDTM 转换函数目录

注册表版本：`1.2.0`

本文件由 `registry-docs` 根据转换注册表生成，请勿手工维护。

| 函数 | 类别 | 阶段 | 模型可选 | 实现 | 说明 | 参数 | 禁用条件 |
|---|---|---|---|---|---|---|---|
| `assign_no_ct` | direct_assignment | field_mapping | TRUE | `sdtm.oak::assign_no_ct` | 将一个不受受控术语约束的来源字段赋给一个目标变量。 |  | 目标变量有受控术语；需要改变值的语义或格式 |
| `hardcode_no_ct` | direct_assignment | field_mapping | TRUE | `sdtm.oak::hardcode_no_ct` | 为不受受控术语约束的目标变量赋固定值。 | value | 目标变量有受控术语；固定值取决于记录条件 |
| `assign_ct` | controlled_terminology | field_mapping | TRUE | `sdtm.oak::assign_ct` | 使用已登记术语表把一个来源字段转换到目标受控术语。 | codelist_id | 目标变量没有受控术语；来源值存在未登记术语 |
| `hardcode_ct` | controlled_terminology | field_mapping | TRUE | `sdtm.oak::hardcode_ct` | 为受控术语目标变量赋经过术语表验证的固定值。 | value \| codelist_id | 固定值不在目标术语表中；固定值取决于未登记条件 |
| `to_iso8601_date` | date_time_conversion | field_mapping | TRUE | `sdtm.oak::create_iso8601` | 将一个能够明确解释的完整日期转换为 ISO 8601 日期。 | formats | 原始值包含不完整日期；日期格式无法确定；只有时间没有日期 |
| `to_iso8601_datetime` | date_time_conversion | field_mapping | TRUE | `sdtm.oak::assign_datetime` | 将同一字段或分开的日期与时间字段转换为完整 ISO 8601 日期时间。 | formats | 日期部分缺失；日期或时间格式存在无法解决的歧义；只有时间没有日期 |
| `to_iso8601_partial_datetime` | date_time_conversion | field_mapping | TRUE | `sdtm.oak::create_iso8601` | 在不进行填补的前提下保留已知日期时间精度。 | formats \| unknown_tokens | 业务规则要求日期填补；只有时间没有日期；未知标记未登记 |
| `merge_sources` | source_integration | source_preparation | TRUE | `TraceSDTM::merge_sources` | 按已登记键和基数关系对两个来源数据集进行确定性连接；该函数输出数据集，target_variables 必须为空，名称写入 output_dataset。 | left_dataset \| right_dataset \| output_dataset \| by \| relationship \| select | 连接键未知；连接会复制基础记录；需要任意 R 连接表达式 |
| `coalesce_fields` | source_integration | field_mapping | TRUE | `TraceSDTM::coalesce_fields` | 按明确优先级从多个候选字段取得第一个非缺失值。 | priority | 来源记录没有可靠连接；优先级未确定 |
| `derive_reference_datetime` | source_integration | field_mapping | TRUE | `sdtm.oak::oak_cal_ref_dates` | 从多个原始数据集选取每名受试者最早或最晚日期时间并派生 DM 参考日期。 | selection \| subject_keys \| sources | 受试者键不一致；参考日期选择规则不明确；来源只有不完整日期 |
| `combine_fields` | field_combination | field_mapping | TRUE | `TraceSDTM::combine_fields` | 按固定分隔符组合多个来源字段。 | separator \| missing_policy | 字段之间没有明确组合规则；来源来自未连接数据集 |
| `derive_usubjid` | field_combination | field_mapping | TRUE | `TraceSDTM::derive_usubjid` | 使用研究编号和受试者编号按批准格式派生 USUBJID。 | separator | 受试者编号构成规则未确定；任一组成字段缺失 |
| `extract_delimited_part` | field_combination | field_mapping | TRUE | `TraceSDTM::extract_delimited_part` | 按固定分隔符和位置抽取标识符的一部分，不接受正则表达式。 | separator \| position | 需要正则表达式；分隔符或位置不稳定 |
| `conditional_assign` | conditional_derivation | field_mapping | TRUE | `TraceSDTM::conditional_assign` | 使用受控条件树进行赋值，不解析任意代码。 | condition \| value | 需要任意 R 表达式；条件依赖未连接数据集 |
| `derive_ongoing_flag` | conditional_derivation | field_mapping | TRUE | `TraceSDTM::derive_ongoing_flag` | 根据结束日期缺失或明确的持续指示派生 ONGOING。 | ongoing_value | 结束日期缺失并不代表持续且没有持续指示；业务规则未确认 |
| `normalize_case` | character_normalization | field_mapping | TRUE | `TraceSDTM::normalize_case` | 将字符值统一为大写或小写。 | case | 大小写改变会改变编码含义；目标需要受控术语映射 |
| `transpose_findings` | record_transposition | record_expansion | TRUE | `TraceSDTM::transpose_findings` | 将一个横向检查结果展开为包含检查代码、名称、原始结果和原始单位的记录。 | test_code \| test_name | 检查代码或名称未确认；结果不是 Findings 结构；单位来源不明确 |
| `standardize_unit` | unit_conversion | post_derivation | TRUE | `TraceSDTM::standardize_unit` | 使用版本化换算集合生成标准字符结果、标准数值和标准单位；受项目政策要求时，相同单位也必须执行身份标准化以保留数值解析、单位确认、舍入和版本追溯。 | conversion_set_id \| target_unit | 模型提供自由公式；原始单位未知；结果不能解释为数值 |
| `derive_sequence` | temporal_derivation | post_derivation | TRUE | `sdtm.oak::derive_seq` | 按受试者和稳定排序字段派生域内连续序号。 | record_variables | 排序变量不足以稳定区分记录 |
| `derive_study_day` | temporal_derivation | post_derivation | TRUE | `sdtm.oak::derive_study_day` | 根据完整目标日期和 DM 参考日期派生研究日，不对不完整日期进行填补。 | target_date \| reference_date | 目标日期或参考日期不完整；受试者无法连接到 DM |
| `derive_visitnum` | temporal_derivation | field_mapping | TRUE | `TraceSDTM::derive_visitnum` | 使用已登记访视表把访视名称转换为数值型访视编号。 | visit_map_id | 访视名称没有批准的数值对应关系 |
| `derive_baseline_flag` | temporal_derivation | post_derivation | TRUE | `sdtm.oak::derive_blfl` | 标记每名受试者每项检查在参考日期前最后一次有效观察。 | reference_date | 参考日期缺失；检查日期不完整；结果缺失或未实施 |
| `do_not_map` | not_used | no_output | TRUE | `TraceSDTM::do_not_map` | 明确记录某个来源概念不进入当前 SDTM 范围。 | reason | 来源概念包含必需 SDTM 信息 |
