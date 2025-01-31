
/// Creates an $X$-spider. 
#let x(

  /// The x coordinate of the spider. 
  /// -> int | float
  x, 

  /// The y coordinate of the spider. 
  /// -> int | float
  y, 

  /// The phase to display. 
  /// -> any
  phase: none
  
) = (
  (
    "kind": "x", 
    x: x, 
    y: y,
    phase: phase
  ),
)

/// Creates an $Z$-spider. 
#let z(

  /// The x coordinate of the spider. 
  /// -> int | float
  x, 

  /// The y coordinate of the spider. 
  /// -> int | float
  y, 

  /// The phase to display. 
  /// -> any
  phase: none
  
) = (
  (
    "kind": "z", 
    x: x, 
    y: y,
    phase: phase
  ),
)


/// Creates a Hadamard gate. 
#let h(

  /// The x coordinate of the Hadamard. 
  /// -> int | float
  x, 

  /// The y coordinate of the Hadamard. 
  /// -> int | float
  y
  
) = (
  (
    "kind": "h", 
    x: x, 
    y: y
  ),
)

