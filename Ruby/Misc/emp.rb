class Employee
    # Adding methods to the singleton class of the object
    # Employee (which has the class Class).
    class << self
      def add(emp)
        puts "Adding #{emp}"
        @employees = Hash.new unless @employees
        @employees[emp.name] = emp
      end
      def get_emp_by_name name
        @employees[name]
      end
    end
    #####################################################
    attr_accessor :name, :ssid, :salary
    def initialize(name, ssid, salary)
      @name = name
      @ssid = ssid
      @salary = salary
      Employee.add self
    end
    def to_s
      @name
    end
  end
  
  alice = Employee.new("Alice Alley", 1234, 75000);
  bob = Employee.new("Robert Tables", 5678, 50000);
  
  class << bob
    def signing_bonus
      2000
    end
  end
  
  puts(bob.signing_bonus);
  #puts(alice.signing_bonus);
  
  b = Employee.get_emp_by_name "Robert Tables"
  puts b.signing_bonus
  
  