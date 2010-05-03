-record(node, {address, nil}).
-record(counter, {name, value}).
-record(tables,{name, nil}). % type :: simple | node_depended

% used to clean mnesia, when some of the nodes are disconnected (in
% master_node module). Also may be used to stop some task (not in this
% version).
-record(used_module,{name, nil}).

-record(local_task, {id, mfa, comment}).

-record(atom_task, {id               % auto
                    , history = []   % auto
                    , run_on_node    % auto
                    , from           % auto
                    , restart = {transient, 4, 2000}
                    , node
                    , link = false
                    , mfa
                    , comment
                   }).

% all row with tag "auto" add automatical
% restart :: {permanent, maxR, maxT} (always restart) | temporary (not
% restart) | {transient, maxR, maxT} (restart only if exit reason
% abnormaly)
% node :: Node
% from :: Pid
% link :: bool()

-record(global_task, {}).
