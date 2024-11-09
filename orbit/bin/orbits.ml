open Oplot

(* Helper function to calculate the points on an ellipse *)
let ellipse_points a b n =
  (* a and b are the semi-major and semi-minor axes *)
  (* n is the number of points to sample along the ellipse *)
  let points = ref [] in
  for i = 0 to n - 1 do
    let theta = (float_of_int i) *. (2.0 *. Float.pi) /. (float_of_int n) in
    let x = a *. cos theta in
    let y = b *. sin theta in
    points := (x, y) :: !points
  done;
  List.rev !points

(* Function to create the plot *)
let plot_orbits a b planet_count =
  (* Generate the points for the star and planets *)
  let star_position = (0.0, 0.0) in
  let orbits = 
    List.init planet_count (fun i ->
      (* We offset the planets along the x-axis for visualization *)
      let orbit_a = a +. (float_of_int i *. 30.0) in
      let orbit_b = b +. (float_of_int i *. 15.0) in
      ellipse_points orbit_a orbit_b 500
    )
  in
  
  (* Create a new plot *)
  let plot = Oplot.create () in
  
  (* Add the star at the center *)
  Oplot.add_point plot star_position ~color:`Yellow ~size:10;
  
  (* Add the orbits and planets *)
  List.iter (fun orbit_points ->
    Oplot.add_points plot orbit_points ~color:`Blue ~size:2
  ) orbits;
  
  (* Display the plot *)
  Oplot.display plot

(* Main function to get user input and generate the plot *)
let () =
  (* Get user input for semi-major axis, semi-minor axis, and planet count *)
  print_endline "Enter the semi-major axis (a) of the orbits:";
  let a = read_float () in
  print_endline "Enter the semi-minor axis (b) of the orbits:";
  let b = read_float () in
  print_endline "Enter the number of planets:";
  let planet_count = read_int () in
  
  (* Plot the orbits *)
  plot_orbits a b planet_count
