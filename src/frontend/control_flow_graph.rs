use super::ast::{self, IfData, Node};

type GraphNodeIndex = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct CFGraph {
    nodes: Vec<CFNode>,
    pub start_node: Option<GraphNodeIndex>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CFNode {
    Start {
        next_node: GraphNodeIndex,
    },
    Node {
        nodes: Vec<Node>,
        branches: Vec<GraphNodeIndex>,
    },
    End,
}

impl From<Vec<Node>> for CFGraph {
    fn from(mut body: Vec<Node>) -> Self {
        let mut graph = CFGraph {
            nodes: vec![],
            start_node: None,
        };

        let root_node = if !body.is_empty() {
            graph.parse_block(&mut body, None)
        } else {
            graph.alloc_node(CFNode::End)
        };
        graph.start_node = Some(graph.alloc_node(CFNode::Start {
            next_node: root_node,
        }));

        graph.clone()
    }
}

impl CFGraph {
    pub fn get_node(&self, index: GraphNodeIndex) -> &CFNode {
        self.nodes.get(index).unwrap()
    }
    pub fn get_node_mut(&mut self, index: GraphNodeIndex) -> &mut CFNode {
        self.nodes.get_mut(index).unwrap()
    }

    fn alloc_node(&mut self, node: CFNode) -> GraphNodeIndex {
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    fn parse_block(
        &mut self,
        body: &mut Vec<Node>,
        parent_block: Option<GraphNodeIndex>,
    ) -> GraphNodeIndex {
        let mut nodes = vec![];
        while !body.is_empty() {
            let node = body.remove(0);
            nodes.push(node);
            if Self::is_node_branching(nodes.last().unwrap()) {
                break;
            }
        }
        let mut branches = match parent_block {
            Some(x) => vec![x],
            None => vec![self.alloc_node(CFNode::End)],
        };
        if let Some(branching_node) = nodes.last_mut() {
            if Self::is_node_branching(branching_node) {
                branches = self.parse_branches(branching_node, body, parent_block)
            }
        }
        self.alloc_node(CFNode::Node { nodes, branches })
    }

    fn parse_branches(
        &mut self,
        branching_node: &mut Node,
        next_nodes: &mut Vec<Node>,
        parent_block: Option<GraphNodeIndex>,
    ) -> Vec<GraphNodeIndex> {
        match branching_node {
            Node::IfStatement(ref mut data) => {
                self.parse_if_statement(data, next_nodes, parent_block)
            }
            _ => panic!("node is not a branch"),
        }
    }

    fn parse_if_statement(
        &mut self,
        data: &mut IfData,
        next_nodes: &mut Vec<Node>,
        parent_block: Option<GraphNodeIndex>,
    ) -> Vec<GraphNodeIndex> {
        let mut nodes = Vec::with_capacity(2);
        let next_block = self.parse_block(next_nodes, parent_block);

        if !data.body.is_empty() {
            nodes.push(self.parse_block(&mut data.body, Some(next_block)));
        } else {
            nodes.push(next_block);
        }
        if !data.else_body.is_empty() {
            nodes.push(self.parse_block(&mut data.else_body, Some(next_block)));
        } else {
            nodes.push(next_block);
        }
        nodes
    }

    fn is_node_branching(node: &Node) -> bool {
        match node {
            Node::IfStatement(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DebugSourceGraphNode {
    Start {
        next_node: Box<DebugSourceGraphNode>,
    },
    Node {
        nodes: Vec<Node>,
        branches: Vec<DebugSourceGraphNode>,
    },
    End,
}

impl CFGraph {
    fn convert_to_debug_node(&self, node: &CFNode) -> DebugSourceGraphNode {
        match node {
            CFNode::Node { nodes, branches } => {
                let mut debug_branches = vec![];
                for branch in branches {
                    debug_branches.push(self.convert_to_debug_node(self.get_node(branch.clone())));
                }
                DebugSourceGraphNode::Node {
                    nodes: nodes.clone(),
                    branches: debug_branches,
                }
            }
            CFNode::End => DebugSourceGraphNode::End,
            CFNode::Start { next_node } => DebugSourceGraphNode::Start {
                next_node: Box::new(self.convert_to_debug_node(self.get_node(next_node.clone()))),
            },
        }
    }
}

impl Into<DebugSourceGraphNode> for CFGraph {
    fn into(self) -> DebugSourceGraphNode {
        let start_node = self.start_node.expect("graph is empty");
        self.convert_to_debug_node(self.get_node(start_node))
    }
}
