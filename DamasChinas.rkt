#lang racket
(require (lib "graphics.ss" "graphics")); se importa la libreria graphics
(open-graphics) ; se abre el modo gráfico

;Se crea la ventana y se pinta de negro
(define canvas (open-viewport "Damas Chinas" 1000 600))
((draw-solid-rectangle canvas)(make-posn 0 0)   800 600  "black")

;Variables para crear el tablero
(define CANVAS-MIDDLE 400)
(define SPACE-BETWEEN-CIRCLES 60)
(define SPACE-BETWEEN-ROWS 30)
(define pos-y 30)
(define initial-position-even-circles 0)
(define initial-position-odd-circles 0)

;Determina la posicion inicial de un circulo, cuando la cantidad de circulos en la fila es PAR
(define (set-even-initial-position circles-quantity)
  (set! initial-position-even-circles
        (- CANVAS-MIDDLE (* (sub1 circles-quantity) 30))))

;Determina la posicion inicial de un circulo, cuando la cantidad de circulos en la fila es IMPAR
(define (set-odd-initial-position circles-quantity)
  (set! initial-position-odd-circles
        (- CANVAS-MIDDLE (* (quotient circles-quantity 2) 60))))

;Dibuja una fila de circulos
(define (draw-circles quantity pos-x)
  (cond [(> quantity 0)
         ((draw-solid-ellipse canvas)(make-posn pos-x pos-y) 30 30 "white")
         (draw-circles (sub1 quantity) (+ pos-x SPACE-BETWEEN-CIRCLES))]
        [else void]))


(define max-circles-row 9)
(define min-circles-row 1)

;Dibuja el tablero de damas chinas
(define (draw-chinesse-checkers-board)
  (cond
    ;Se dibuja de arriba hacia abajo
    [(and (< min-circles-row 9) (even? min-circles-row))
     (set-even-initial-position min-circles-row) 
     (draw-circles min-circles-row initial-position-even-circles)
     (set! min-circles-row (add1 min-circles-row)) ;se aumenta la cantidad de circulos para la siguiente fila
     (set! pos-y (+ pos-y SPACE-BETWEEN-ROWS)) ;se aumenta la posicion de siguente fila
     (draw-chinesse-checkers-board)]
    
    [(and (< min-circles-row 9) (odd? min-circles-row))
     (set-odd-initial-position min-circles-row)
     (draw-circles min-circles-row initial-position-odd-circles)
     (set! min-circles-row (add1 min-circles-row))
     (set! pos-y (+ pos-y SPACE-BETWEEN-ROWS))
     (draw-chinesse-checkers-board)]

    [(and (> max-circles-row 0) (even? max-circles-row))
     (set-even-initial-position max-circles-row)
     (draw-circles max-circles-row initial-position-even-circles)
     (set! max-circles-row (sub1 max-circles-row))
     (set! pos-y (+ pos-y SPACE-BETWEEN-ROWS))
     (draw-chinesse-checkers-board)]
    
    [(and (> max-circles-row 0) (odd? max-circles-row))
     (set-odd-initial-position max-circles-row)
     (draw-circles max-circles-row initial-position-odd-circles)
     (set! max-circles-row (sub1 max-circles-row))
     (set! pos-y (+ pos-y SPACE-BETWEEN-ROWS))
     (draw-chinesse-checkers-board)]

    [else void]))

(draw-chinesse-checkers-board)



