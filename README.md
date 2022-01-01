# Kiwi Lang

Scripting Language made for fun

## Example

```
def hello_name name {
    return "Hello " + name
}

def main {
    integer = 1
    floating_point = 2.53
    string = "Hello World!"
    tuple = (1, 3.14)
    array = [1, 2, 3, 4]
    object = {
        key: "value"
    }

    if 1 < 2 {
        print "1 is smaller than 2"
    } else if 2 > 1 {
        print "1 is greater than 2"
    } else {
        print "1 is equal to 2"
    }

    print string  // Calling methods without parens, parens are for tuples
    hello_name "Oded" | print  // Chaining, prints "Hello Oded" 
}
```