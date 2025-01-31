#let quad(
  control, 
  end,
  relative: false
) = (
  control-start: control,
  control-end: control,
  end: end,
  relative: relative
)


#let cubic(
  control-start,
  control-end,
  end,
  relative: false
) = (
  control-start: control-start,
  control-end: control-end,
  end: end,
  relative: relative
)


#let line(
  end,
  relative: false,
) = (
  control-start: none,
  control-end: none,
  end: end,
  relative: relative
)
