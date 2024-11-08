open Oplot.Plt

type orbiting_body = {
  mass : float;
  semi_major_axis : float;
  eccentricity : float;
  true_anomaly : float;
  inclination : float;
  argument_of_periapsis : float;
  longitude_of_ascending_node : float;
};;

type central_body = {
  mass : float;
  gravitational_constant : float;
};;

let input_float prompt =
  Printf.printf "%s:\n" prompt;
  flush stdout;
  Scanf.scanf "%f\n" (fun x -> x)
;;

let get_gravitational_constant () = 6.674e-11;;

let input_int prompt =
  Printf.printf "%s:\n" prompt;
  flush stdout;
  Scanf.scanf "%d\n" (fun x -> x)
;;

let input_orbiting_body () =
  let mass = input_float "Enter the mass of the orbiting body (kg)" in
  let semi_major_axis = input_float "Enter the semi-major axis (meters)" in
  let eccentricity = input_float "Enter the eccentricity (0 to 1)" in
  let true_anomaly = input_float "Enter the true anomaly (radians)" in
  let inclination = input_float "Enter the inclination (radians)" in
  let argument_of_periapsis = input_float "Enter the argument of periapsis (radians)" in
  let longitude_of_ascending_node = input_float "Enter the longitude of ascending node (radians)" in
  {mass; semi_major_axis; eccentricity; true_anomaly; inclination; argument_of_periapsis; longitude_of_ascending_node}
;;

let calculate_ellipse_points body num_points =
  let a = body.semi_major_axis in
  let e = body.eccentricity in
  Array.init num_points (fun i ->
    let theta = 2. *. Float.pi *. (float_of_int i /. float_of_int num_points) in
    let r = a *. (1. -. e *. e) /. (1. +. e *. cos theta) in
    let x = r *. cos theta in
    let y = r *. sin theta in
    (x, y)
  )
;;

let plot_ellipse points =
  let plot_data = Array.to_list points in
  let scatter = Scatter.scatter ~data:plot_data () in
  Plot.display [scatter]
;;

let () = 
  let central_mass = input_float "Enter the mass of the central body (kg)" in
  let central_body = {mass = central_mass; gravitational_constant = get_gravitational_constant ()} in
  let num_bodies = input_int "Enter the number of orbiting bodies" in 
  
  Printf.printf "Number of orbiting bodies: %d\nGravitational Constant: %f\nCentral Body Mass: %f\n" 
    num_bodies central_body.gravitational_constant central_body.mass;
  flush stdout;

  let orbiting_bodies = ref [] in
  for i = 1 to num_bodies do
    Printf.printf "\nEnter details for orbiting body %d\n" i;
    let body = input_orbiting_body () in
    orbiting_bodies := !orbiting_bodies @ [body]
  done;

  for i = 0 to List.length !orbiting_bodies - 1 do
    let body = List.nth !orbiting_bodies i in
    Printf.printf "\n- Orbiting Body %d \n- Mass: %f kg\n- Semi-Major Axis: %f meters\n- Eccentricity: %f\n- True Anomaly: %f radians\n- Inclination: %f radians\n- Argument of Periapsis: %f radians\n- Longitude of Ascending Node: %f\n"
      (i + 1) body.mass body.semi_major_axis body.eccentricity body.true_anomaly body.inclination body.argument_of_periapsis body.longitude_of_ascending_node
  done;

  List.iter (fun body ->
    let points = calculate_ellipse_points body 100 in
    plot_ellipse points
  ) !orbiting_bodies
;;
