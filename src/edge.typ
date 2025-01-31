#import "utility.typ": is-curve-element, is-spider

#let get-main-node(node) = {
  if is-curve-element(node) {
    node
  } else if is-spider(node.at(0)) { 
    node.first()
  } else if type(node) == array and type(node.at(0)) == array and is-spider(node.at(0).at(0)) {
    node.at(0).at(0)
  } else { 
    node
  }
}


/// Creates a chain of edges between a series of nodes. These nodes can be 
/// - spiders, like `zx.x`,
/// - coordinates given as an array, e.g., `(1, 2)`
/// - curve elements such as `zx.line`, `zx.quad`, and `zx.cubic`. 
#let edge(

  /// The nodes to connect. 
  /// -> spider | array | line | quad | cubic
  ..nodes,

  
  /// Output direction of the first node, given either as an `angle`,
  /// an `alignment` or an `array` with one of these two and an
  /// additional strength scalar (the order in the array does not 
  /// matter). This gives the edge a curved look
  ///
  /// Can only be used for exactly 2 spiders or coordinates. 
  ///
  /// -> auto | alignment | angle | array
  from: auto,

  
  /// Input direction of the second node, see @edge.from. 
  ///
  /// -> auto | alignment | angle | array
  to: auto
  
) = {



  if nodes.named().len() != 0 {
    assert(false, message: "Unexpected argument " + nodes.named().keys().first())
  }
  
  assert(from == auto or type(from) in (alignment, angle, array))
  assert(to == auto or type(to) in (alignment, angle, array))

  
  (
    (nodes: nodes.pos().map(get-main-node), from: from, to: to),
  )
}



/// Creates a chain of edges between a series of nodes with Hadamards on 
/// each edge. See @edge. 
#let h-edge(

  /// The nodes to connect. 
  /// -> spider | array | line | quad | cubic
  ..nodes,
  
  /// Output direction of the first node, see @edge.from. 
  ///
  /// -> auto | alignment | angle | array
  from: auto,
  
  /// Input direction of the second node, see @edge.from. 
  ///
  /// -> auto | alignment | angle | array
  to: auto,

  /// The position of the Hadamard
  at: .5
  
) = {
  
  if nodes.named().len() != 0 {
    assert(false, message: "Unexpected argument " + nodes.named().keys().first())
  }
  assert(from == auto or type(from) in (alignment, angle))
  assert(to == auto or type(to) in (alignment, angle))

  
  (
    (nodes: nodes.pos().map(get-main-node), from: from, to: to, at: at),
  )
}
