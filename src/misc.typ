#import "utility.typ": is-edge, is-content, is-spider
#let place(
  x, 
  y, 
  body,
  align: center + horizon
) = {
  (
    (x: x, y: y, body: body, align: align ),
  )
}



#let group(
  dx: 0,
  dy: 0,
  ..children
) = {
  
  if children.named().len() != 0 {
    assert(false, message: "Unexpected argument " + nodes.named().keys().first())
  }
  
  children = children.pos().join()
  if dx == 0 and dy == 0 {
    return children
  }
  children.map(child => {
    if is-edge(child) {
      child.nodes = child.nodes.map(node => {
        node.x += dx
        node.y += dy
        node
      })
    } else if is-spider(child) or is-content(child) {
      child.x += dx
      child.y += dx
    }
    child
  })
}