#import "utility.typ": is-spider, is-edge, is-content, is-curve-element
#import "spiders.typ": h

#import "curve.typ" as zx-curve

#let vec-sub(x, y) = x.zip(y).map(((p, q)) => q - p)
#let vec-add(x, y) = x.zip(y).map(((p, q)) => q + p)
#let vec-inv(x) = x.map(q => -q)


/// Evaluates a Bézier curve of any degree for a given curve 
/// parameter value. 
#let evaluate-bezier(

  /// Bézier control points (absolute). 
  /// -> array
  ..points, 
  
  /// Curve parameter. Must be between 0 and 1. 
  /// -> float
  t: 0.5
  
) = {
  let interpolate(x, y) = {
    x.zip(y).map(((p, q)) => p + t * (q - p))
  }
  
  let points = points.pos()
  while points.len() > 1 {
    points = points.windows(2).map(p => interpolate(..p))
  }
  return points.first()
}


/// Draws a spider. 
#let draw-spider(

  /// The spider object to draw. 
  /// -> spider
  spider, 

  /// Length of one unit. 
  /// -> length
  scale, 

  /// Outline stroke of the spider. 
  /// -> stroke
  stroke: 1pt
  
) = {
  let fill = if spider.kind == "x" { 
    red.lighten(70%) } 
    else if spider.kind == "z" { green.lighten(70%) } 
    else { yellow.lighten(70%) }
    
  let phase = spider.at("phase", default: none)
  if phase == none {
    phase = []
    // phase = square(stroke: none, width: 2pt)
    
  } else {
    phase = $ inline(phase) $ // ensure correct bounds measuring
  }
  // phase = box(phase, stroke: 1pt)
  let body 
  if spider.kind == "h" {
    body = rect(
      fill: fill, stroke: stroke, 
      height: .5em, width: .5em, 
    )
  } else {
    body = rect(
      phase, 
      fill: fill, stroke: stroke, 
      inset: 5pt, radius: 100%
    )
  }
  return body
}


/// Turns a direction given by an angle or an alignment
/// to an angle always. 
/// -> angle
#let direction-to-angle(
  
  /// -> angle | alignment
  direction
  
) = {
  if type(direction) == angle { return direction }
  
  assert(direction in (left, right, top, bottom))
  if direction == right { return 0deg }
  if direction == left { return 180deg }
  if direction == top { return -90deg }
  if direction == bottom { return 90deg }
}


/// Update a bounds dictionary with a set of points. 
#let update-bounds(
  
  /// Bounds to update. If `none`, the bounds of all given points
  /// are computed. 
  /// -> none | dictionary
  bounds, 

  /// Points or bounds to extend the bounds with. 
  /// -> array | dictionary
  ..points
  
) = {
  for point in points.pos() {
    if bounds == none {
      if type(point) == array {
        let (x, y) = point
        bounds = (left: x, right: x, top: y, bottom: y)
      } else {
        bounds = point
      }
    } else {
      if type(point) == array {
        let (x, y) = point
        bounds.left = calc.min(bounds.left, x)
        bounds.right = calc.max(bounds.right, x)
        bounds.top = calc.min(bounds.top, y)
        bounds.bottom = calc.max(bounds.bottom, y)
      } else {
        bounds.left = calc.min(bounds.left, point.left)
        bounds.right = calc.max(bounds.right, point.right)
        bounds.top = calc.min(bounds.top, point.top)
        bounds.bottom = calc.max(bounds.bottom, point.bottom)
      }
    }
  }
  return bounds
}

/// Turns Bézier (cubic) coordinates with optional relative 
/// handles to polygon with up to 4 absolute (!) points. 
/// -> array
#let bezier-coords-to-polygon(
  
  /// Start point like `((1, 2),)` or start point
  /// with relative Bézier handle `((1, 2), (.2, .2))`. 
  /// -> array
  start, 
  
  /// End point like `((1, 2),)` or end point
  /// with relative Bézier handle `((1, 2), (.2, .2))`. 
  /// -> array
  end
  
) = {
  return start + end.filter(x => x != none)
  if start.len() == 2 {
    start.last() = vec-sub(..start.rev())
  }
  if end.len() == 2 {
    end.last() = vec-add(..end)
  }
  return start + end.rev()
}


/// Adds a Bézier handle to an end point
#let maybe-add-bezier-handle(

  /// Coordinates of the end point. 
  /// -> array
  point, 

  /// Direction of the handle, given either as an `angle`,
  /// an alignment or an array with one of these two and an
  /// additional strength scalar. The order in the array does not 
  /// matter. 
  /// -> angle | alignment | array
  direction, 

  /// Use `invert: true` for the start point to mirror the handle
  /// as required for `std.path`.
  /// -> bool
  invert: false
  
) = {
  if direction == auto { 
    if type(point.first()) in (int, float) {
      return (point,)
    } else {
      return point
    }
  }

  if type(point.first()) not in (int, float) {
    return point
  }
  
  let strength = 1
  if type(direction) == array {
    assert(direction.len() == 2)
    for element in direction {
      if type(element) in (angle, alignment) {
        direction = element
      } else if type(element) in (int, float) {
        strength = element
      } else {
        assert(false)
      }
    }
  }
  let angle = direction-to-angle(direction)
  if invert { angle += 180deg }
  let handle = (calc.cos(angle), calc.sin(angle))
  
  (point, handle.map(x => x / 2 * strength))
}

#let process-direction(
  /// -> angle | alignment | array
  direction,

  vertex

) = {
  if direction == auto { return none }
  
  let strength = 1
  if type(direction) == array {
    assert(direction.len() == 2)
    for element in direction {
      if type(element) in (angle, alignment) {
        direction = element
      } else if type(element) in (int, float) {
        strength = element
      } else {
        assert(false)
      }
    }
  }
  let angle = direction-to-angle(direction)
  let handle = (calc.cos(angle), calc.sin(angle))
  vec-add(vertex, handle.map(x => x / 2 * strength))
}

/// Adds a Bézier handle to an end point
#let maybe-add-bezier-handle2(

  start, 
  
  /// Coordinates of the end point. 
  /// -> array
  end, 

  /// Direction of the handle, given either as an `angle`,
  /// an alignment or an array with one of these two and an
  /// additional strength scalar. The order in the array does not 
  /// matter. 
  /// -> angle | alignment | array
  from, 
  
  /// -> angle | alignment | array
  to, 
  
) = {
  if from == auto and to == auto { return end }
  let control-start = process-direction(from)
  let control-end = process-direction(to)

  return (control-start, control-end, end)
}


/// Draws a helper grid for orientation at all integer positions. 
#let draw-grid(

  /// A dictionary of scalar bounds for `top`, `bottom`, `left`,
  /// and `right`. 
  /// -> dictionary
  bounds, 

  /// Length of one unit. 
  /// -> length
  scale
  
) = {
  set line(stroke: .3pt + gray)
  set text(.5em, gray)
  for x in range(calc.ceil(bounds.left), calc.floor(bounds.right) + 1) {
    let dy = bounds.top * scale
    place(line(start: (x * scale, dy), length: 100%, angle: 90deg))
    place(dx: x * scale, dy: dy - .3em, place(center + bottom)[#x])
  }
  for y in range(calc.ceil(bounds.top), calc.floor(bounds.bottom + 1)) {
    let dx = bounds.left * scale
    place(line(start: (dx, y * scale), length: 100%))
    place(dx: dx - .3em, dy: y * scale, place(right + horizon)[#y])
  }
}


/// Places something and also compute its bounds. 
#let place-with-bounds(

  /// Content to place
  /// -> any
  body, 

  /// Amount to shift the content along the x axis. 
  /// -> int | float
  dx: 0, 

  /// Amount to shift the content along the x axis. 
  /// -> int | float
  dy: 0, 

  
  /// Length of one unit. 
  /// -> length
  scale: 10pt,

  /// How to align the body to the coordinates `dx` and `dy`.
  /// -> alignment
  align: center + horizon
  
) = {

  if align.x == none { align = align + center }
  else if align.y == none { align = align + horizon }

  let size = measure(body)
  
  let body = place(align, body, dx: dx * scale, dy: dy * scale)
  
  if align.x == right { dx -= size.width / scale }
  else if align.x == center { dx -= 0.5 * size.width / scale }
  if align.y == bottom { dy -= size.height / scale }
  else if align.y == horizon { dy -= 0.5 * size.height / scale }
  
  let bounds = (
    left: dx, 
    right: dx + size.width / scale,
    top: dy,
    bottom: dy + size.height / scale
  )
  
  
  (body, bounds)
}



#let extract-spider(node) = {
  if is-spider(node) {
    return node
  } else if is-curve-element(node) {
    if is-spider(node.end.at(0)) {
      return node.end.at(0)
    }
  }
}


#let ensure-curve-element(node) = {
  if is-spider(node) {
    node = zx-curve.line((node.x, node.y))
  } else if is-curve-element(node) {
    if is-spider(node.end.at(0)) {
      node.end = (node.end.at(0).x, node.end.at(0).y)
    }
  } else if type(node) == array and type(node.first()) in (int, float) {
    node = zx-curve.line(node)
  }
  return node
}


/// Creates a ZX-diagram. 
#let diagram(
  
  /// Edges or spiders
  /// -> edge | spider
  items, 
  
  /// Length of one unit. 
  /// -> length
  scale: 25pt, 
  
  /// Stroke for edges and spiders. 
  /// -> stroke
  stroke: 0.5pt, 
  
  /// Whether to display a helper grid with numbers for rows
  /// and columns
  /// -> bool
  grid: true
  
) = context {
  set math.equation(numbering: none)
  
  let spiders = ()
  let bounds = none

  let diagram-body = {
      
    for item in items + () {
      if is-spider(item) {
        
        spiders.push(item)
        
      } else if is-content(item) {
        
        let (body, item-bounds) = place-with-bounds(
          dx: item.x,
          dy: item.y,
          item.body,
          align: item.align,
          scale: scale
        )
        bounds = update-bounds(bounds, item-bounds)
        body
        
      } else if is-edge(item) { 
  
        let edge = item
        let nodes = edge.nodes

        spiders += nodes.map(extract-spider).filter(x => x != none)


        if edge.nodes.len() <= 1 { continue }  
        if edge.nodes.len() > 2 {
          assert(edge.to == auto, message: "`zx.edge.to` should be `auto` when constructing edges between more than two nodes")
          assert(edge.from == auto, message: "`zx.edge.from` should be `auto` when constructing edges between more than two nodes")
        }

        
        if edge.from != auto or edge.to != auto {
          assert(not nodes.any(is-curve-element), message: "`zx.edge.to` and `zx.edge.from` should not be used with curve elements")
          let extract-coord(node) = if is-spider(node) { (node.x, node.y) } else { node
        }
          let (start, end) = nodes.map(extract-coord)
          let control-start = process-direction(edge.from, start)
          let control-end = process-direction(edge.to, end)
          nodes = (
            start,
            zx-curve.cubic(control-start, control-end, end)
          )
        }

        let curve-elements = nodes.map(ensure-curve-element)

        let coords = curve-elements.map(
          node => (node.control-start, node.control-end, node.end)
        )

        // Insert all automatic coordinates (symmetric handles)
        for i in range(1, coords.len()) {
          if coords.at(i).first() == auto {
            let prev = coords.at(i - 1)
            if prev.at(1) == none {
              coords.at(i).first() = none
              continue
            }
            let rel = vec-sub(prev.at(2), prev.at(1))
            coords.at(i).first() = vec-sub(rel, prev.at(2))
          }
        }

        coords = coords.map(el => el.filter(x => x != none))
        
        if "at" in edge { // is a Hadamard edge
          spiders += coords.windows(2).map(
            ((start, end)) => {
              let pos = evaluate-bezier(start.last(), ..end, t: edge.at)
              h(..pos)
            }
          ).join()
        }
        
        
        bounds = update-bounds(
          bounds, ..coords.join()
        )

        
        let apply-scaling = p => { p.map(el => scale * el) }

        let curve-elements = coords.enumerate().map(
          ((i, node)) => {
            node = node.map(apply-scaling)
            if i == 0 { curve.move(..node) }
            else if node.len() == 1 { curve.line(..node) }
            else if node.len() == 2 { curve.quad(..node) }
            else if node.len() == 3 { curve.cubic(..node) }
          }
        )
        

        place(
          curve(..curve-elements, stroke: stroke)
        )


        
      } else {
        assert(false, message: "Unknown thing " + repr(item))
      }
    }
  
    spiders = spiders.dedup(key: spider => (spider.x, spider.y))
    {
      
      for spider in spiders {
        let (x, y) = (spider.x, spider.y)
        
        let body = draw-spider(spider, scale, stroke: stroke)
        let size = measure(body)
        
        bounds = update-bounds(
          bounds, 
          (x - size.width/2/scale, y - size.height/2/scale),
          (x + size.width/2/scale, y + size.height/2/scale),
        )
        
        place(
          dx: x * scale,
          dy: y * scale,
          place(center + horizon, body)
        )
      }
    }
  }
  
  if bounds == none { 
    bounds = (left: 0, right: 0, top: 0, bottom: 0)
  }

  let bounds-pt = bounds.pairs()
    .map(((key, val)) => (key, val * scale))
    .to-dict()


  block(
    // stroke: .1pt, 
    breakable: false,
    width: bounds-pt.right - bounds-pt.left,
    height: bounds-pt.bottom - bounds-pt.top,
    move(
      dx: -bounds-pt.left, 
      dy: -bounds-pt.top, 
      {
        if grid { draw-grid(bounds, scale) }
        diagram-body
      }
    )
  )
}
