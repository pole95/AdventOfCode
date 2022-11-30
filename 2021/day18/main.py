import json


class Node:
    def __init__(self, left, right):
        self.left = left
        self.right = right
        self.parent = None
        self.value = None
        left.parent = self
        right.parent = self

    def __str__(self):
        return "[" + str(self.left) + "," + str(self.right) + "]"

    def get_left_neighbor(self):
        parent = self.parent
        node = self
        while parent is not None:
            if parent.right == node:
                # start searching rightest child of left element
                r = parent.left
                while r is not None:
                    if type(r) is Leaf:
                        return r
                    else:
                        r = r.right
            else:
                node = parent
                parent = node.parent
        # has no left neighbor
        return None

    def get_right_neighbor(self):
        parent = self.parent
        node = self
        while parent is not None:
            if parent.left == node:
                # start searching leftest child of right element
                l = parent.right
                while l is not None:
                    if type(l) is Leaf:
                        return l
                    else:
                        l = l.left
            else:
                node = parent
                parent = node.parent
        # has no right neighbor
        return None


class Leaf:
    def __init__(self, value):
        self.value = value
        self.parent = None

    def __str__(self):
        return str(self.value)


def split(node: Node):
    left = node.value // 2
    right = node.value // 2 + (node.value % 2)
    n = Node(Leaf(left), Leaf(right))
    n.parent = node.parent
    if node.parent.left == node:
        node.parent.left = n
    elif node.parent.right == node:
        node.parent.right = n


def explode(node: Node):
    left_neighbor = node.get_left_neighbor()
    right_neighbor = node.get_right_neighbor()
    if left_neighbor is not None:
        left_neighbor.value = left_neighbor.value + node.left.value
    if right_neighbor is not None:
        right_neighbor.value = right_neighbor.value + node.right.value
    n = Leaf(0)
    n.parent = node.parent
    if node.parent.left == node:
        node.parent.left = n
    if node.parent.right == node:
        node.parent.right = n


def get_split(node: Node) -> Node:
    if type(node) is Leaf:
        if node.value >= 10:
            return node
        else:
            return None
    else:
        split_left = get_split(node.left)
        if split_left is not None:
            return split_left
        split_right = get_split(node.right)
        if split_right is not None:
            return split_right
    return None


def get_exploding(node: Node, depth=0) -> Node:
    if type(node) is Leaf:
        return None
    elif depth == 4:
        return node
    else:
        # try lef side first
        _l = get_exploding(node.left, depth+1)
        if _l is not None:
            return _l
        # then go right
        _r = get_exploding(node.right, depth+1)
        if _r is not None:
            return _r
    return None


def reduce(node: Node) -> Node:
    changed = True
    while changed:
        changed = False
        node_to_explode = get_exploding(node)
        if node_to_explode is not None:
            explode(node_to_explode)
            changed = True
            continue
        node_to_split = get_split(node)
        if node_to_split is not None:
            split(node_to_split)
            changed = True
            continue
    return node


def node_from_list(x):
    if type(x) is not list:
        return Leaf(x)
    else:
        l = node_from_list(x[0])
        r = node_from_list(x[1])
        return Node(l, r)


def add(node1, node2):
    return Node(node1, node2)


def magnitude(node):
    if type(node) is Leaf:
        return node.value
    else:
        return 3*magnitude(node.left) + 2*magnitude(node.right)


def task1(file):
    with open(file, "r") as inputs:
        number = node_from_list(json.loads(inputs.readline().strip("\n")))
        for line in inputs:
            new_number = node_from_list(json.loads(line.strip("\n")))
            # add
            number = add(number, new_number)
            reduce(number)
        return magnitude(number)


def task2(file):
    largest_magnitude = 0
    number1 = None
    number2 = None
    with open(file, "r") as inputs:
        numbers = [x.strip() for x in inputs]
        for n1 in numbers:
            for n2 in numbers:
                if n1 == n2:
                    continue
                number1 = n1
                number2 = n2
                _n1 = node_from_list(json.loads(n1))
                _n2 = node_from_list(json.loads(n2))
                largest_magnitude = max(largest_magnitude, magnitude(reduce(add(_n1, _n2))))
    return largest_magnitude

print(task1("input.true"))
print(task2("input.true"))
