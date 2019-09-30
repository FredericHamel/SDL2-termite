
(define-library (SDL2-termite)
  (export make-sdl2-node make-screen-node)

  ; (import (scheme base))
  (import (gambit)) ;; define-macro

  (import (SDL2))
  (import (SDL2 ttf))

  (import (termite))

  (begin
    (define (make-sdl2-node)
      (spawn
        (lambda ()
          (let loop ((screens '()) (ttf #f))
            (recv
              (('init subsystem)
               (SDL_Init subsystem)
               (loop screens))

              (('init-ttf)
               (TTF_Init)
               (loop screens '()))

              (('register screen-node)
               (loop (if (memq screen-node screens)
                         screens
                         (cons screen-node screens))))

              (('open-font name size)
               (loop screens (and ttf
                                  (cons
                                    (cons (cons name size)
                                          (TTF_OpenFont name size))
                                    ttf))))

              ((from tag ('get-font name size))
               (! from (list tag (or ttf (assoc (cons name size) ttf)))))

              ('quit
               (for-each
                 (lambda (node)
                   (! node 'quit)
                   (wait-for node))
                 screens)
               (if (not ttf)
                   (begin
                     (for-each
                       (lambda (name/font)
                         (TTF_Close (cdr name/font)))
                       ttf)
                     (TTF_Quit)))
               (SDL_Quit)))))))

    (define (make-screen-node title x y w h mode)
      (let* ((win (SDL_CreateWindow title x y w h mode))
             (renderer (SDL_CreateRenderer win -1 SDL_RENDERER_ACCELERATED)))
        (spawn
          (lambda ()
            (let loop ((timeout (/ 1 60.0))) ; 60 Hz
              (recv
                (('timeout val)
                 (loop val))

                ('toggle-cursor
                 (SDL_ToggleCursor renderer)
                 (loop timeout))

                ('clear
                 (SDL_RenderClear renderer))

                ('quit
                 (SDL_DestroyRenderer renderer)
                 (SDL_DestroyWindow win))

                (('set-draw-color color) (u8vector? color)
                 (SDL_SetRenderDrawColor renderer color)
                 (loop timeout))

                (('draw-text font text rect)
                 (TTF_RenderUTF8Solid font text

                (('draw-rect rect) (where (s32vector? rect))
                 (SDL_RenderFillRect renderer rect)
                 (loop timeout))

                (msg
                  (pp msg)
                  (loop timeout))

                (after timeout
                  (SDL_RenderPresent renderer)
                  (loop timeout)))))))
          name: (string->symbol title))))))

