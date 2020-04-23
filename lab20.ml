type image = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type size = int * int ;;
open Graphics ;;

(* threshold thershold image -- image where pixels above the threshold
value are black *)
let threshold img threshold =
  List.map (fun row -> List.map (fun pix -> if pix <= threshold then 0. else 1.)
                                 row) img

(* show the image *)
let depict img =
  Graphics.open_graph "";
  let w, ht = List.length (List.hd img), List.length img in
    Graphics.resize_window w ht;
  let depict_pix v r c = let lvl = int_of_float (255. *. (1. -. v)) in
    Graphics.set_color (Graphics.rgb lvl lvl lvl);
  plot c (ht - r) in
  List.iteri (fun r row -> List.iteri (fun c pix -> depict_pix pix r c) row) img;
  Unix.sleep 2;
  Graphics.close_graph () ;;

(* dither max image -- dithered image *)
let dither = List.map (fun row -> List.map
                      (fun pix -> if pix > Random.float 1. then 1. else 0.) row)

let mona = Monalisa.image ;;
depict mona ;;

let mona_threshold = threshold mona 0.75 ;;
depict mona_threshold ;;

let mona_dither = dither mona ;;
depict mona_dither ;;

