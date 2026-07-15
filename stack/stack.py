def new():
    return []

def push(stack, elem):
    return stack + [elem]

def peek(stack):
    return stack[-1]

def pop(stack):
    elem = peek(stack)
    new_stack = stack[:-1]
    return (elem, new_stack)

def add(stack):
    n1,s1 = pop(stack)
    n2,s2 = pop(s1)
    return push(s2, n1 + n2)

s = new()
s1 = push(s,1)
s2 = push(s1,1)
s3 = add(s2)
s4 = push(s3,3)
s5 = add(s4)
print(s5)
