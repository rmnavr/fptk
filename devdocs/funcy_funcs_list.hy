
	Sequences
		"Create"			count cycle repeat repeatedly iterate re_all re_iter
		"Access"			first second last nth some take
		"Slice"			    take drop rest butlast takewhile dropwhile split_at split_by
		"Transform"		    map/lmap mapcat keep pluck pluck_attr invoke
		"Filter"			filter/lfilter remove keep distinct where without
		"Join"			    cat concat flatten mapcat interleave interpose
		"Partition"		    chunks partition partition_by split_at split_by
		"Group"			    split count_by count_reps group_by group_by_keys group_values
		"Aggregate"		    ilen reductions sums all any none one count_by count_reps
		"Iterate"		    pairwise with_prev with_next zip_values zip_dicts tree_leaves tree_nodes

	Collections
		"Join"			    merge merge_with join join_with
		"Transform"		    walk walk_keys walk_values
		"Filter"			select select_keys select_values compact
		"Dicts*"			flip zipdict pluck where itervalues iteritems zip_values zip_dicts project omit
		"Misc"			    empty get_in get_lax set_in update_in del_in has_path

	Functions
		"Create"			identity constantly func_partial partial rpartial iffy caller re_finder re_tester
		"Transform"		    complement iffy autocurry curry rcurry
		"Combine"		    compose rcompose juxt/ljuxt all_fn any_fn none_fn one_fn some_fn

	Other topics
		"Content tests"	    all any none one is_distinct
		"Type tests"		isa is_iter is_list is_tuple is_set is_mapping is_seq is_seqcoll is_seqcont iterable
		"Decorators"		decorator wraps unwrap autocurry
		"Control flow"	    once once_per once_per_args collecting joining post_processing throttle wrap_with
		"Error handling"	retry silent ignore suppress limit_error_rate fallback raiser reraise
		"Debugging"		    tap log_calls log_enters log_exits log_errors log_durations log_iter_durations
		"Caching"		    memoize cache cached_property cached_readonly make_lookuper silent_lookuper
		"Regexes"		    re_find re_test re_all re_iter re_finder re_tester
		"Strings"		    cut_prefix cut_suffix str_join
		"Objects"		    cached_property cached_readonly wrap_prop monkey invoke pluck_attr LazyObject
		"Primitives"		isnone notnone inc dec even odd


