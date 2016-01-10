(require 'derived)

(defvar sdc-keyword '("all_clocks" "all_inputs" "all_outputs" "all_registers" "create_clock" "create_generated_clock" "derive_clocks" "get_cells" "get_clocks" "get_nets" "get_pins" "get_ports" "remove_clock_groups" "remove_clock_latency" "remove_clock_uncertainty" "remove_disable_timing" "remove_input_delay" "remove_output_delay" "reset_design" "set_clock_groups" "set_clock_latency" "set_clock_uncertainty" "set_disable_timing" "set_false_path" "set_input_delay" "set_input_transition" "set_max_delay" "set_min_delay" "set_multicycle_path" "set_output_delay" ) )
(defvar sdc-ext-keyword '( "derive_clock_uncertainty" "derive_pll_clocks" "get_assignment_groups" "get_fanins" "get_fanouts" "get_keepers" "get_nodes" "get_partitions" "get_registers" "remove_annotated_delay" "remove_clock" "set_annotated_delay" "set_max_skew" "set_net_delay" "set_scc_mode" ) ) 

(defvar sdc-sta-keyword '( "check_timing" "create_slack_histogram" "create_timing_netlist" "create_timing_summary" "delete_timing_netlist" "enable_ccpp_removal" "enable_sdc_extension_collections" "get_available_operating_conditions" "get_cell_info" "get_clock_domain_info" "get_clock_fmax_info" "get_clock_info" "get_datasheet" "get_default_sdc_file_names" "get_edge_info" "get_edge_slacks" "get_min_pulse_width" "get_net_info" "get_node_info" "get_object_info" "get_operating_conditions" "get_operating_conditions_info" "get_partition_info" "get_path" "get_path_info" "get_pin_info" "get_point_info" "get_port_info" "get_register_info" "get_timing_paths" "locate" "query_collection" "read_sdc" "report_advanced_io_timing" "report_bottleneck" "report_clock_fmax_summary" "report_clock_transfers" "report_clocks" "report_datasheet" "report_ddr" "report_exceptions" "report_max_skew" "report_metastability" "report_min_pulse_width" "report_net_delay" "report_net_timing" "report_partitions" "report_path" "report_rskm" "report_sdc" "report_tccs" "report_timing" "report_ucp" "set_operating_conditions" "set_time_format" "timing_netlist_exist" "update_timing_netlist" "use_timequest_style_escaping" "write_sdc"))

(define-derived-mode sdc-mode tcl-mode "sdc"
"Major mode for editing SDC files. Special commands: \\ quip-mode-map}"
;(define-key sdc-mode-map (kbd "C-'") 'sdc-list-cmd)
(make-local-variable 'sdc-keyword-list)
(setq sdc-keyword-list (append tcl-keyword-list sdc-keyword sdc-sta-keyword) )
(append tcl-keyword-list sdc-keyword-list)
(tcl-set-font-lock-keywords)
)


(defun sdc-mode-list-cmd ()
  ""
  (interactive)
  ( tcl-help-on-word "sta")
  )



(provide 'sdc-mode)