-record(node, {address, nil}).
-record(local_task, {id, m, f, a, comment}).
-record(counter, {name, value}).
-record(tables,{name, nil}). % type :: simple | node_depended
-record(used_module,{name, nil}).



-record(atom_task, {}).
-record(global_task, {}).
