

type orbiting_body = {
  mass : float;            (* Mass of the orbiting body (kg) *)
  semi_major_axis : float; (* Semi-major axis (meters) *)
  eccentricity : float;    (* Eccentricity (0 to 1) *)
  true_anomaly : float;    (* True anomaly (radians) *)
  inclination : float;     (* Inclination (radians) *)
  argument_of_periapsis : float; (* Argument of periapsis (radians) *)
  longitude_of_ascending_node : float; (* Longitude of ascending node (radians) *)
};;

type central_body = {
  mass : float;           (* Mass of the central body (kg) *)
  gravitational_constant : float; (* Gravitational constant *)
};;

type current_time = {
  year : int;
  month : int;
  day : int;
};;


let input_float prompt =
  Printf.printf "%s:\n " prompt;
  Scanf.scanf "%f\n" (fun x -> x);

;;

let input_int prompt =
  Printf.printf "%s: \n" prompt;
  Scanf.scanf "%d\n" (fun x -> x);

;;

let input_orbiting_body () =
  let mass = input_float "Enter the mass of the orbiting body (kg)" in
  let semi_major_axis = input_float "Enter the semi-major axis (meters)" in
  let eccentricity = input_float "Enter the eccentricity (0 to 1)" in
  let true_anomaly = input_float "Enter the true anomaly (radians)" in
  let inclination = input_float "Enter the inclination (radians)" in
  let argument_of_periapsis = input_float "Enter the argument of periapsis (radians)" in
  let longitude_of_ascending_node = input_float "Enter the longitude of ascending node (radians)" in
  let body = {mass; semi_major_axis; eccentricity; true_anomaly; inclination; argument_of_periapsis; longitude_of_ascending_node} in
  (* This is just so the error doesn't happen in that it's unused*)
  let _ = body.mass in
  let _ = body.semi_major_axis in
  let _ = body.eccentricity in
  let _ = body.true_anomaly in
  let _ = body.inclination in
  let _ = body.argument_of_periapsis in
  let _ = body.longitude_of_ascending_node in
  body
;;
  
let () =
  let _ = input_orbiting_body() in 
  let today = { year = 2024; month = 10; day = 31 } in
  let central_mass = input_float "Enter the mass of the central body (kg)" in
  let central_body = {mass = central_mass; gravitational_constant = 6.674e-11} in
  let num_bodies = input_int "Enter the number of orbiting bodies" in 
  Printf.printf "Today is: %d / %d / %d\n" today.month today.day today.year;
  Printf.printf "Number of orbiting bodies: %d\nGravitational Constant: %f\nCentral Body Mass: %f\n" 
    num_bodies central_body.gravitational_constant central_body.mass;
  
