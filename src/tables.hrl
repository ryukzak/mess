-record(node, {address, nil}).
-record(counter, {name, value}).
-record(tables,{name, nil}). % type :: simple | node_depended
-record(used_module,{name, nil}).

-record(local_task, {id, m, f, a, comment}).

-record(atom_task, {id % auto
                    , restart = transient
                    , maxR = 4
                    , maxT = 2000
                    , currentR = 0 % auto
                    , node 
                    , run_on_node % auto
                    , from % auto
                    , exit_msg
                    , m, f, a, comment}).

% all row with tag "auto" add automatical

% restart :: permanent (always restart) | temporary (not restart) |
% transient (restart only if exit reason abnormaly)
% node :: Node
% from :: Pid
% exit_msg :: subscribe | undefined
% maxR, maxT

-record(global_task, {}).
