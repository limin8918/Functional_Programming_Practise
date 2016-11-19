def strict(b: Boolean, i: Int) = if (b) i+i else 0
strict(true, {println("hi"); 41+1})

def nonStrict(b: Boolean, i: => Int) = if (b) i+i else 0
nonStrict(true, {println("hi"); 41+1})