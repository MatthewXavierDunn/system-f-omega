putStrLn ""
putStrLn "Examples of records"
putStrLn ""

assume (Student :: *)
assume (Student :: String -> Nat -> Student)

-- accessors
let getName (Student name _) = name :: (Student -> String)
let getAge (Student _ age) = age :: (Student -> Nat)

-- setters
let setName name (Student _ age) = Student name age :: String -> Student -> Student
let setAge age (Student name _) = Student name age :: Nat -> Student -> Student

-- construct the person record
let adam = Student "Adam" 22

putStrLn "Making an instance of the Student record:"
print adam
putStrLn ""

putStrLn "Accessors:"

putStrLn "getName adam ="
print getName adam

putStrLn "getAge adam ="
print getAge adam
putStrLn ""

putStrLn "Now let's change Adam to Steve using the setters:"
putStrLn "setAge 24 (setName \"Steve\" adam) ="
let steve = setAge 24 (setName "Steve" adam)

print steve

assume (Staff :: *)
assume (Staff :: String -> Staff)

putStrLn ""
putStrLn "Records can be nested"
assume (University :: *)
assume (University :: List Staff -> List Student -> University)

-- accessors
let getStaff (University staff _) = staff :: University -> List Staff
let getStudents (University _ students) = students :: University -> List Student

putStrLn ""
putStrLn "Making St. Andrews:"
putStrLn "University [Staff \"StaffMember\"] (take 5 (repeat (Student \"Student\" 20))) ="
let stAndrews = University [Staff "StaffMember"] (take 5 (repeat (Student "Student" 20)))
print stAndrews
putStrLn ""

putStrLn "Getting each of the nested values:"
putStrLn "getStaff stAndrews ="
print getStaff stAndrews
putStrLn ""
putStrLn "getStudents stAndrews ="
print getStudents stAndrews

putStrLn ""
putStrLn "And records can be dove into:"
putStrLn "getName (head (getStudents stAndrews)) ="
print getName (head (getStudents stAndrews))
