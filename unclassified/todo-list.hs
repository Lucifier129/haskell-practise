data Todo = Todo {
  content :: String,
  completed :: Bool
} deriving (Show)


initialState = 
  [
    Todo { content = "text1", completed = False },
    Todo { content = "text2", completed = False },
    Todo { content = "text3", completed = False },
    Todo { content = "text4", completed = False },
    Todo { content = "text5", completed = False }
  ]


data Action =
  AddTodo String
  |
  UpdateTodo String String
  |
  RemoveTodo String
  |
  ToggleTodo String
  |
  ToggleAll Bool


type TodoList = [Todo]

reducer :: Action -> TodoList  -> TodoList
reducer action todoList = case action of
  (AddTodo text) -> todoList ++ [Todo { content = text, completed = False }]

  (UpdateTodo old new) -> [ updateContent old new todo | todo <- todoList ]
     
  (RemoveTodo text) -> [ todo | todo <- todoList, content todo /= text ]

  (ToggleTodo text) -> [ toggleTodo text todo | todo <- todoList ]

  (ToggleAll completed) -> [ todo { completed = completed } | todo <- todoList ]

updateContent :: String -> String -> Todo -> Todo
updateContent oldText newText todo
  | content todo == oldText = todo { content = newText }
  | otherwise = todo

toggleTodo :: String -> Todo -> Todo
toggleTodo text todo
  | content todo == text = todo { completed = not (completed todo) }
  | otherwise = todo
