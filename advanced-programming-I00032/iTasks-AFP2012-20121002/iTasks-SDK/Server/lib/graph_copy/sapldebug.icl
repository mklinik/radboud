implementation module sapldebug

import StdEnv,graph_to_sapl_string, StdDebug,graph_to_string_with_descriptors

sapldebug :: !a .b -> .b
sapldebug a b = trace_n ("DEBUG: " +++ graph_to_sapl_string a) b

