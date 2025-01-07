projectpath <- "~/stoxprosjekter/analyticalCAA/"
bl<-RstoxFramework::getProcessTable(projectpath, "baseline")
a<-RstoxFramework::getProcessTable(projectpath, "analysis")
r<-RstoxFramework::getProcessTable(projectpath, "report")

makeProcessGraph <- function(processTable, graph=NULL, color="black", internalfillcolor="white"){
  if (is.null(graph)){
    graph <- DiagrammeR::create_graph(attr_theme = "tb")    
    graph <-   DiagrammeR::add_global_graph_attrs(graph,
      attr = "overlap",
      value = "false",
      attr_type = "graph")

  }
  
  for (i in 1:nrow(processTable)){
    
    nodecolor <- color
    fillcolor <- internalfillcolor
    
    
    if (processTable$terminalProcess[[i]]){
      fillcolor <- color
    }
    if (length(processTable$functionInputs[[i]])==0){
      fillcolor <- color
    }
    
    toNode <- processTable$processName[i]
    if (is.na(DiagrammeR::get_node_ids(graph, conditions = label==toNode))){
      graph <- DiagrammeR::add_node(graph, label = toNode,
                                    node_aes = DiagrammeR::node_aes(
                                      color = nodecolor,
                                      shape="box",
                                      fixedsize = FALSE
                                      )
                                    )      
    }

    
      for (input in processTable$functionInputs[[i]]){
        fromNode <- input
        if (is.na(DiagrammeR::get_node_ids(graph, conditions = label==fromNode))){
          graph <- DiagrammeR::add_node(graph, label = fromNode,
                                        node_aes = DiagrammeR::node_aes(
                                          color = nodecolor,
                                          shape="box",
                                          fixedsize = FALSE)
                                        )    
        }
        graph <- DiagrammeR::add_edge(graph, 
                                      DiagrammeR::get_node_ids(graph, conditions = label==fromNode),
                                      DiagrammeR::get_node_ids(graph, conditions = label==toNode)
                                      )
      }
  }
  
  
  return(graph)
}

graph <- makeProcessGraph(bl, color="blue")
graph <- makeProcessGraph(a, graph = graph, color = "red")
graph <- makeProcessGraph(r, graph = graph, color = "green")
DiagrammeR::render_graph(graph, title = projectpath)