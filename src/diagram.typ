#import "utility.typ": is-spider, is-edge, is-content
#import "spiders.typ": h

#import "curve.typ": quad, cubic

#let vec-sub(x, y) = x.zip(y).map(((p, q)) => q - p)
#let vec-add(x, y) = x.zip(y).map(((p, q)) => q + p)


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

        spiders += edge.nodes.filter(node => type(node) == dictionary)

        let retrieve-coordinates(node) = {
          if type(node) == dictionary { 
            (node.x, node.y) 
          } else if type(node) == array and  type(node.at(0)) == dictionary {
            let spider = node.at(0)
            ((spider.x, spider.y),) + node.slice(1)
          } else { 
            node
          }
        }
        
        let nodes = edge.nodes
          .map(retrieve-coordinates)

        if nodes.len() <= 1 { continue }  
        if nodes.len() > 2 {
          assert(edge.to == auto, message: "`zx.edge.to` should be `auto` when constructing edges between more than two nodes")
          assert(edge.from == auto, message: "`zx.edge.from` should be `auto` when constructing edges between more than two nodes")
        }
        let previous = nodes.first()
        let path-coords = ()

        for node in nodes.slice(1) {
            
            let start = maybe-add-bezier-handle(previous, edge.from, invert: true)
            let end = maybe-add-bezier-handle(node, edge.to)
            
            if "at" in edge {
              spiders += h(
                ..evaluate-bezier(
                  ..bezier-coords-to-polygon(start, end), 
                  t: edge.at
                )
              )
            }

            bounds = update-bounds(
              bounds, ..bezier-coords-to-polygon(start, end)
            )
            if path-coords.len() == 0 {
              path-coords.push(start)
            }
            path-coords.push(end)
            previous = node
  
        }
        let point-to-coords = p => p.map(el => scale * el)

        place(path(
          ..path-coords.map(path-coord => path-coord.map(point-to-coords)),
          stroke: stroke)
        )

        
        
        // if type(from-points.first()) in (int, float) {
        //   from-points = (from-points,)
        // } else {
        //   spiders += edge.a.filter(x => type(x) == dictionary)
        // }
        
        // if type(to-points.first()) in (int, float) {
        //   to-points = (to-points,)
        // } else {
        //   spiders += edge.b.filter(x => type(x) == dictionary)
        // }
        
        let to-coord = node => if type(node) == dictionary { (node.x, node.y) } else { node }
  
        
        // for start in from-points.map(to-coord) {
        //   for end in to-points.map(to-coord) {
            
        //     if "at" in edge {
        //       spiders += h(..evaluate-bezier(start, end, t: edge.at))
        //     }
  
        //     start = maybe-add-bezier-handle(start, edge.from, invert: true)
        //     end = maybe-add-bezier-handle(end, edge.to)
            
        //     bounds = update-bounds(
        //       bounds, ..bezier-coords-to-polygon(start, end)
        //     )

        //     let point-to-coords = p => p.map(el => scale * el)
        //     place(path(
        //       start.map(point-to-coords), 
        //       end.map(point-to-coords), 
        //       stroke: stroke)
        //     )
            
        //   }
        // }
        
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
