import sys


class File:

    def __init__(self, name, size = 0) -> None:
        self.name = name
        self.size = size
    
    def __str__(self) -> str:
        return f'({self.name}, {self.size})'


class Folder(File):

    def __init__(self, name, parent) -> None:
        self.children = dict()
        self.parent = parent
        super().__init__(name)
    

    def __str__(self) -> str:
        temp = ', '.join(map(str, self.children.values()))
        return f'{{({self.name}, {self.size}): {temp}}}'


    def compute_sizes(self) -> None: 
        
        def find_size(file):
            if isinstance(file, Folder):
                file.compute_sizes()
            return file.size

        self.size = sum(map(find_size, self))
    

    def __iter__(self):
        return self.children.values().__iter__()


def parse(lines: list[str]) -> Folder:
    lines.pop(0)
    result = Folder('/', None)
    ptr = result

    for string in lines:
        if string == '$ ls':
            continue
        elif 'dir ' in string:
            _, name = string.split('dir ')
            ptr.children[name] = Folder(name, ptr)
        elif string[0].isdigit():
            size, name = string.split(' ')
            ptr.children[name] = File(name, int(size))
        elif '$ cd ..' == string:
            ptr = ptr.parent
        elif '$ cd ' in string:
            _, name = string.split('cd ')
            ptr = ptr.children[name]
    
    return result


def solve_1(file: Folder) -> int:
    if not isinstance(file, Folder):
        return 0
    
    result = sum(map(solve_1, file))
    
    if file.size <= 100_000:
        result += file.size

    return result


def solve_2(file: Folder, min_size) -> int:
    
    if file.size >= min_size and isinstance(file, Folder):
        minimum = min(map(lambda x: solve_2(x, min_size), file))
        return min(file.size, minimum)

    return float('inf')


def main():
    lines = sys.stdin.read().split('\n')
    root: Folder = parse(lines)
    root.compute_sizes()
    print(solve_1(root))
    max_size = 30_000_000 - (70_000_000 - root.size)
    print(solve_2(root, max_size))


if __name__ == '__main__':
    main()