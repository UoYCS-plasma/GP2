module GraphViz where


import Graph
import GP2Graph

gvExportNode :: GP2Graph -> NodeId -> String
gvExportNode g n = show $ nLabel g n

