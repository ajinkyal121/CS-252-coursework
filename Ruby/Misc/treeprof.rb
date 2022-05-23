class Tree
    attr_accessor :value, :left, :right
    def initialize(value, left=nil, right=nil)
      @value = value
      @left = left
      @right = right
    end
    def each_node(&each)
        left.each_node(&each) if left
        yeild @value
        right.each_node(&each) if right
    end
    def get_node(path)
        next_step = path.shift
        if !next_step then 
            @value
        elsif next_step == "left" then
            left.get_node(path)
        else
            right.get_node(path)
        end
    end
    def method_missing(m, *args)
        path = m.to_s.scan(/left|right/)
        get_node path
    end
  end
  
  my_tree = Tree.new(42,
                     Tree.new(3,
                              Tree.new(1,
                                       Tree.new(7,
                                                Tree.new(22),
                                                Tree.new(123)),
                                       Tree.new(32))),
                     Tree.new(99,
                              Tree.new(81)))
  
  my_tree.each_node do |v|
    puts v
  end
  
  arr = []
  my_tree.each_node do |v|
    arr.push v
  end
  p arr
  
  p "Getting nodes from tree"
  p my_tree.left_left
  p my_tree.right_left
  p my_tree.left_left_right
  p my_tree.left_left_left_right