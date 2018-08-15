type Weight = Double
type Node = [Weight]
type Layer = [Node]
type Network = [Layer]

type Input = Double
type Inputs = [Input]

type Node_Output = Double
type Layer_Output = [Node_Output]
type Network_Output = [Layer_Output]

type Label = Double
type Labels = [Double]

type Error_Input = Double
type Error_Inputs = [Error_Input]

type Node_Error = Double
type Layer_Errors = [Node_Error]
type Network_Error = [Layer_Errors]

type Activation = Double -> Double
type Derivative = Double -> Double

sigmoid :: Activation
sigmoid x = 1 / (1 + exp (-x / 1))

sigmoid_derivative :: Derivative
sigmoid_derivative x = x * (1 - x)

create_node :: Int -> Node
create_node weight_count = [ 0 | _ <- [0..weight_count] ]

create_layer :: Int -> Int -> Layer
create_layer input_count node_count = [ create_node input_count | _ <- [0..node_count-1] ]

create_network :: [Int] -> Network
create_network (input_count:node_count:xs) = [create_layer input_count node_count] ++ create_network (node_count : xs)
create_network _ = []

compute_node :: Node -> Inputs -> Activation -> Node_Output
compute_node (bias:weights) inputs activation = activation $ bias + (sum [w * i | (w, i) <- zip weights inputs])

compute_layer :: Layer -> Inputs -> Activation -> Layer_Output
compute_layer layer inputs activation = [ compute_node node inputs activation | node <- layer ]

compute_network :: Network -> Inputs -> Activation -> Network_Output
compute_network (layer:network') inputs activation = let output = compute_layer layer inputs activation in [inputs] ++ compute_network network' output activation
compute_network _ inputs _ = [inputs]

compute_node_error :: Node -> Error_Inputs -> Node_Error
compute_node_error (bias:weights) (bias_error:weight_errors) = bias_error + sum [ weight * error | (weight, error) < zip weights weight_errors]

compute_layer_error :: Layer -> Error_Inputs -> Layer_Errors
compute_layer_error layer error_inputs = [compute_node_error node error_inputs | node <- layer]

compute_network_error :: Network -> Network_Output -> Labels -> Network_Error
compute_network (layer:network') network_output labels = 
  let
    error_inputs = [output - label | (label, output) <- zip labels (head.reverse network_output) ]
    layer_error = compute_layer_error layer error_inputs
    in
    [layer_error] ++ compute_network network' 

      
