export make = (vals...) => 
  obj = {}
  vals.forEach (val) =>
    obj[val] = val
  obj