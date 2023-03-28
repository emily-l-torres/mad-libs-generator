;;Emily Torres
;;Principles of Programming Languages
;;Conference Project: Mad Libs Generator

;; Resources:
;; Credit to Mad Takes (online knockoff of Mad Libs) for How to Make PB&J, Pizza Parlor, Green Tea Label and History of Sonic the HedgeHog Mad Libs stories:
;; https://www.madtakes.com/
;; Best Friends, Going to the Store, and Sick were written (poorly) by me.

(display "~Welcome to the Mad Lib Generator!~\n\n")
(display "Choose from one of the following Mad Libs:\n\n")
(display "* Best Friends ['best-friends-mad-lib]\n")
(display "* Sick ['sick-mad-lib]\n")
(display "* Pizza Parlor ['pizza-mad-lib]\n")
(display "* How To Make a PB&J ['pbj-mad-lib]\n")
(display "* Green Tea Label ['green-tea-mad-lib]\n")
(display "* History of Sonic the HedgeHog ['sonic-mad-lib]\n")
(display "* Going to the Store ['store-mad-lib]\n\n")
(display "To generate a Mad Lib, please type (mad-lib '[title])\n")
(display "Example: (mad-lib 'hello-mad-lib)\n")

;;Helper functions
(define get-symbol
  (lambda (n input-list)
    (cond
      [(equal? n 0) (car input-list)]
      [else (get-symbol (- n 1) (cdr input-list))])))

(define subst-one
  (lambda (new old input-list)
    (cond
      [(null? input-list) '()]
      [(equal? (car input-list) old) (cons new (cdr input-list))]
      [else (cons (car input-list) (subst-one new old (cdr input-list)))])))

(define concat
  (lambda (list1 list2)
    (cond
      [(null? list1) list2]
      [(null? list2) list1]
      [else (cons (car list1) (concat (cdr list1) list2))])))

;;Lists of Words
(define noun-list
  '(ball fan computer car chair flag house plate bed table
         teapot bottle lanyard jacket RGB-keyboard pillow microphone shoe thermometer cart
         piano bowl spoon dragon seed beard sock purse knife fork
         playbill textbook lamp cushion ID mask couch TV rug outlet
         mirror eyeliner fire straw box poster clock watch notebook hourglass
         switch gate bread dough flour necklace bracelet mug diaper ice-cream
         keychain sheet cheese salt pepper slipper trash-can sink soap shaving-cream
         flower towel bookshelf water shampoo conditioner phone alarm-clock glove window
         succulent figurine book pen pencil refrigerator oven stuffed-animal drawer perfume
         program sweater earbuds hand-sanitizer case suitcase tape glasses water-bottle expo-marker))

(define verb-list
  '(dance swing smack walk run kick wave eat drink sleep
          skip slide write draw mail squeeze pound hang fold close
          open soak rip fly fall scream wash dry wring slam
          click push pull turn brush comb spin staple tape cut
          glue copy scan edit trip sign sip tap pay charge
          punch knock spit play scratch stretch sketch enter read press
          calculate nap die evaporate shine drive speed bang yawn print
          kiss hiss hug cry smile cook fry throw catch type
          sing sneeze cough scream help soar wish call sniff smell
          wait bake ride listen text pack bounce step yell browse))

(define verb-ing-list
  '(dancing swinging smacking walking running kicking waving eating drinking sleeping
            skipping sliding writing drawing mailing squeezing pounding hanging folding closing
            opening soaking ripping flying falling screaming washing drying wringing slamming
            clicking pushing pulling turning brushing combing spinning stapling taping cutting
            gluing copying scanning editing tripping signing sipping tapping paying charging
            punching knocking spitting playing scratching stretching sketching entering reading pressing
            calculating napping dying evaporating shining driving speeding banging yawning printing
            kissing hissing hugging crying smiling cooking frying throwing catching typing
            singing sneezing coughing screaming helping launching wishing calling sniffing smelling
            waiting baking riding listening texting packing bouncing stepping yelling browsing))

(define adjective-list
  '(cute small big ugly bad good smelly soft hard rough
         gross delicious scary difficult easy beautiful pretty colorful fragile opaque
         musty crusty dusty fake real loose tight noisy quiet broken
         shiny matte dirty greasy colorful loud quiet gooey sticky bright
         minty itchy cold hot lukewarm loose tight lazy wet dry
         pointy smooth dim long short skinny fat slim scary brave
         frozen pungent conscious clumsy gainful useful earthy cheap cynical fair
         rhetorical marvelous charming charismatic satisfying cultured offbeat serious relaxed filthy
         lush finicky wise curvy old young mindless possible aggressive nimble
         dizzy macho stingy utter fertile remarkable rustic festive pricey erratic))

(define plural-noun-list
  '(keys flags trees maps cars dogs cats wolves groceries cacti
         yachts bottles jetskis lanyards sunglasses pants shirts swimsuits drinks dishes
         nails tacks blankets pillows beds desks chairs lights keyboards books
         masks towels banners characters mattresses microphones jackets clocks cups alarm-clocks
         boxes badges cameras hoodies fans earbuds trains stories cloths clothes
         bowls muffins croissants teas ovens refrigerators packages notes windows shades
         glasses sheets plushies rugs lamps capes rolls post-its screens monitors
         mice lotions palettes wipes medicines monocles hats hangers hooks robes
         soaps toilets sinks counters cabinets couches chairs tables caps liquids
         socks shoes pets dogs cats boots pictures programs messages apps))

(define verb-ed-list
  '(danced killed smacked walked gathered kicked waved grabbed gulped snoozed
           skipped fixed cleaned shaded mailed squeezed pounded twisted folded closed
           opened soaked ripped bolted nailed screamed washed dried tickled slammed
           clicked pushed pulled turned brushed combed climbed stapled taped fanned
           glued copied scanned edited tripped signed sipped tapped alerted charged
           punched knocked melted played scratched stretched sketched entered glanced pressed
           calculated napped died evaporated glistened bled sped banged yawned printed
           kissed hissed hugged cried smiled cooked fried launched received typed
           belted sneezed coughed screamed helped soared wished called sniffed smelled
           waited baked glided listened texted packed bounced stepped yelled browsed))

(define food-list
  '(pasta meatballs sandwich ramen sushi riceballs cannoli tiramisu chicken-nuggets fries
          rice steak ribs corn green-beans turkey ham cookies garlic bread
          cupcake pumpkin tofu burger butter flour salt pepper mushroom lobster
          artichoke tomato lettuce cheese mayo ketchup mustard soup grilled-cheese beef
          spaghetti croquette kimchi pork pbj yogurt apple orange banana strawberry
          grape watermelon kiwi dragonfruit passionfruit broccoli salami onion herring shrimp
          tuna salmon eel tilapia gherkin halibut roll cinnamon-roll cake doughnut/donut
          veal liver bruschetta oyster clam mackerel asparagus potato tangerine fish
          octopus stew casserole Jello muffin scone spinach sashimi edamame ice-cream
          brownie chips chicken-tenders risotto pickle lollipop marshmallow chocolate vanilla mango))

(define verb-s-list
  '(dances swings smacks walks runs kicks waves eats drinks sleeps
          skips slides writes draws mails squeezes pounds hangs folds closes
          opens soaks rips flies falls screams washes dries wrings slams
          clicks pushes pulls turns brushes combs spins staples tapes cuts
          glues copys scans edits trips signs sips taps pays charges
          punches knocks spits plays scratches stretches sketches enters reads presses
          calculates naps dies evaporates shines drives speeds bangs yawns prints
          kisses hisses hugs cries smiles cooks fries throws catches types
          sings sneezes coughs screams helps soars wishes calls sniffs smells
          waits bakes rides listens texts packs bounces steps yells browses))
 
;;Random Filler Words for Mad Libs
(define random-noun1 (get-symbol (random 100) noun-list))
(define random-noun2 (get-symbol (random 100) noun-list))
(define random-noun3 (get-symbol (random 100) noun-list))
(define random-noun4 (get-symbol (random 100) noun-list))
(define random-noun5 (get-symbol (random 100) noun-list))
(define random-noun6 (get-symbol (random 100) noun-list))
(define random-noun7 (get-symbol (random 100) noun-list))
(define random-noun8 (get-symbol (random 100) noun-list))
(define random-noun9 (get-symbol (random 100) noun-list))
(define random-noun10 (get-symbol (random 100) noun-list))

(define random-verb1 (get-symbol (random 100) verb-list))
(define random-verb2 (get-symbol (random 100) verb-list))
(define random-verb3 (get-symbol (random 100) verb-list))
(define random-verb4 (get-symbol (random 100) verb-list))
(define random-verb5 (get-symbol (random 100) verb-list))

(define random-verb-ing1 (get-symbol (random 100) verb-ing-list)) 
(define random-verb-ing2 (get-symbol (random 100) verb-ing-list))
(define random-verb-ing3 (get-symbol (random 100) verb-ing-list))
(define random-verb-ing4 (get-symbol (random 100) verb-ing-list))

(define random-adjective1 (get-symbol (random 100) adjective-list))
(define random-adjective2 (get-symbol (random 100) adjective-list))
(define random-adjective3 (get-symbol (random 100) adjective-list))
(define random-adjective4 (get-symbol (random 100) adjective-list))
(define random-adjective5 (get-symbol (random 100) adjective-list))

(define random-color (get-symbol (random 10) '(red blue orange yellow green pink purple black white brown)))

(define random-name (get-symbol (random 20) '(Bethany Jim Emily Max Elizabeth Skyler Shane Heather Tammy Brittney
                                                      Ashley Gina Katie Olivia Jane Christen Jennifer Abby Grace John)))

(define random-plural-noun1 (get-symbol (random 100) plural-noun-list))
(define random-plural-noun2 (get-symbol (random 100) plural-noun-list))

(define random-liquid (get-symbol (random 10) '(water juice coffee soda smoothie wine soup milk lemonade tea)))

(define random-verb-ed1 (get-symbol (random 100) verb-ed-list))

(define random-verb-s1 (get-symbol (random 100) verb-s-list))

(define random-food (get-symbol (random 100) food-list))

(define random-integer (random 1000))

(define random-body-part-plural (get-symbol (random 10) '(ears noses mouths eyes heads legs arms hands feet toes)))

(define random-letter1 (get-symbol (random 26) '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))
(define random-letter2 (get-symbol (random 26) '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))

(define random-place (get-symbol (random 20) '(Japan Korea Brazil USA Nigeria China Russia Mexico Guatemala Puerto-Rico
                                                     Dominican-Republic Cuba Canada Iceland Hungary Germany United-Kingdom Ireland Scotland Spain)))

;;Best Friends Mad Lib
(define best-friends-mad-lib
  (lambda (line)
    (cond
      [(equal? 'best-friends-1 line) (subst-one random-adjective1 '____ '(Be kind to your ____ friends.))]
      [(equal? 'best-friends-2 line) (subst-one random-adjective2 '____ '(For you may be someones ____ friend.))]
      [(equal? 'best-friends-3 line) (subst-one random-verb1 '____ '(Be kind to the friends that ____ you from time to time.))]
      [(equal? 'best-friends-4 line) (subst-one random-noun1 '1____ (subst-one random-verb2 '2____ '(Check in on them once in a while -- Maybe they need a/an 1____ or someone to 2____ with.)))]
      [(equal? 'best-friends-5 line) (subst-one random-verb3 '____ '(You guys may ____ sometimes and thats okay.))]
      [(equal? 'best-friends-6 line) (subst-one random-adjective3 '1____ (subst-one random-noun2 '2____. '(But no matter what you will always be each others 1____ 2____.)))])))

(define best-friends-pt1 (concat (best-friends-mad-lib 'best-friends-1) (concat (best-friends-mad-lib 'best-friends-2) (best-friends-mad-lib 'best-friends-3))))
(define best-friends-pt2 (concat (best-friends-mad-lib 'best-friends-4) (concat (best-friends-mad-lib 'best-friends-5) (best-friends-mad-lib 'best-friends-6))))

(define best-friends
  (lambda ()
    (display (concat best-friends-pt1 best-friends-pt2))))

;;Sick Mad Lib
(define sick-mad-lib
  (lambda (line)
    (cond
      [(equal? 'sick-1 line) '(I got the worst cold this weekend.)] 
      [(equal? 'sick-2 line) (subst-one random-noun1 '1____ (subst-one random-adjective2 '2____ '(It felt like I had a/an 1____ in my sinuses and my fever burned hotter than a/an 2____ pepper.)))]
      [(equal? 'sick-3 line) (subst-one random-verb-ing1 '____ '(I tried to go to bed early, but my neighbor was ____ until 2am.))]
      [(equal? 'sick-4 line) (subst-one random-noun2 '____ '(So I got up to make ____ because my mom always said that helps.))]
      [(equal? 'sick-5 line) (subst-one random-noun3 '____ '(After eating, I fell asleep watching a docu-series called ____ : A Documentary.))]
      [(equal? 'sick-6 line) (subst-one random-noun4 '1____ (subst-one random-noun5 '2____ '(It must have affected my dreams because I woke up in a cold sweat after dreaming about a/an 1____ with a/an 2____ on it.)))]
      [(equal? 'sick-7 line) '(I just hope I’m better tomorrow!)])))

(define sick-pt1 (concat (sick-mad-lib 'sick-1) (concat (sick-mad-lib 'sick-2) (concat (sick-mad-lib 'sick-3) (sick-mad-lib 'sick-4)))))
(define sick-pt2 (concat (sick-mad-lib 'sick-5) (concat (sick-mad-lib 'sick-6) (sick-mad-lib 'sick-7))))

(define sick
  (lambda ()
    (display (concat sick-pt1 sick-pt2))))

;;Going to the Store Mad Lib
(define store-mad-lib
  (lambda (line)
    (cond
      [(equal? 'store-1 line) '(Today, I need to go to the store.)] 
      [(equal? 'store-2 line) (subst-one random-noun1 '1____ (subst-one random-noun2 '2____ (subst-one random-noun3 '3____ '(I packed my 1____ and 2____ in my 3____ and I’m off!))))]
      [(equal? 'store-3 line) (subst-one random-noun4 '1____ (subst-one random-noun5 '2____ (subst-one random-noun6 '3____ (subst-one random-noun7 '4____ '(What do I need? I need 1____ 2____ 3____ 4____ and Oreos.)))))]
      [(equal? 'store-4 line) (subst-one random-noun8 '1____ (subst-one random-noun9 '2____ '(I should also pick up 1____ for dinner tonight. Oh! I also need to pick up a/an 2____ for the family picnic.)))]
      [(equal? 'store-5 line) (subst-one random-adjective1 '____ '(I got everything I needed and I’m heading back home. What a/an ____ day!))])))

(define store-pt1 (concat (store-mad-lib 'store-1) (concat (store-mad-lib 'store-2) (store-mad-lib 'store-3))))
(define store-pt2 (concat (store-mad-lib 'store-4) (store-mad-lib 'store-5)))

(define store
  (lambda ()
    (display (concat store-pt1 store-pt2))))

;;Pizza Parlor Mad Lib
(define pizza-mad-lib
  (lambda (line)
    (cond
      [(equal? 'pizza-1 line) (subst-one random-name '1____ (subst-one random-adjective1 '2____ '(Come on over to 1____ ’s Pizza Parlor where you can enjoy you favorite 2____ -dish pizzas.)))]
      [(equal? 'pizza-2 line) (subst-one random-adjective2 '1____ (subst-one random-adjective3 '2____ (subst-one random-noun1 '3____ (subst-one random-noun2 '4____ '(You can try our famous 1____ -lovers pizza, or select from our list of 2____ toppings, including sauteed 3____ 4____ and many more.)))))]
      [(equal? 'pizza-3 line) (subst-one random-verb-ed1 '1____ (subst-one random-liquid '2____ (subst-one random-noun3 '3____'(Our crusts are hand- 1____ and basted in 2____ to make them seem more 3____ -made.))))])))

(define pizza-all (concat (pizza-mad-lib 'pizza-1) (concat (pizza-mad-lib 'pizza-2) (pizza-mad-lib 'pizza-3))))

(define pizza
  (lambda ()
    (display pizza-all)))

;;How To Make a PB&J Mad Lib
(define pbj-mad-lib
  (lambda (line)
    (cond
      [(equal? 'pbj-1 line) '(Step 1: Select the type of bread you want to use.)]
      [(equal? 'pbj-2 line) (subst-one random-color '1____ (subst-one random-noun1 '2____ '(Many prefer the taste of 1____ bread while others prefer 2____ bread because it is healthy.)))]
      [(equal? 'pbj-3 line) (subst-one random-food '____ '(Step 2: Choose the flavor of Jam/Jelly. I personally prefer ____ jam, but you can use whatever you want.))]
      [(equal? 'pbj-4 line) (subst-one random-adjective1 '1____ (subst-one random-adjective2 '2____ '(Step 3: Choose the type of peanut butter - either 1____ or 2____)))]
      [(equal? 'pbj-5 line) (subst-one random-integer '____ '(Step 4: Take out ____ slice(s) of bread.))]
      [(equal? 'pbj-6 line) (subst-one random-noun2 '1____ (subst-one random-verb1 '2____ '(Step 5: Use a/an 1____ to 2____ the jam all over the pieces of bread.)))]
      [(equal? 'pbj-7 line) (subst-one random-verb2 '____ '(Step 6: Now ____ the peanut butter on the other piece of bread.))]
      [(equal? 'pbj-8 line) (subst-one random-noun3 '____ '(Step 7: Put them together and you have a PB&J ____))])))

(define pbj-pt1 (concat (pbj-mad-lib 'pbj-1) (concat (pbj-mad-lib 'pbj-2) (concat (pbj-mad-lib 'pbj-3) (pbj-mad-lib 'pbj-4)))))
(define pbj-pt2 (concat (pbj-mad-lib 'pbj-5) (concat (pbj-mad-lib 'pbj-6) (concat (pbj-mad-lib 'pbj-7) (pbj-mad-lib 'pbj-8)))))

(define pbj
  (lambda ()
    (display (concat pbj-pt1 pbj-pt2))))

;;Green Tea Mad Lib
(define green-tea-mad-lib
  (lambda (line)
    (cond
      [(equal? 'green-tea-1 line) (subst-one random-adjective1 '1____ (subst-one random-verb-ed1 '2____ (subst-one random-adjective2 '3____ (subst-one random-adjective3 '4____ (subst-one random-plural-noun1 '5____ (subst-one random-adjective4 '6____ (subst-one random-plural-noun2 '7____ '(This 1____  green tea is 2____ with the highly 3____  4____ flavor of tart green 5____ 'and the 6____  sweet flavor of orange 7____ ))))))))]
      [(equal? 'green-tea-2 line) (subst-one random-verb-s1 '1____ (subst-one random-adjective5 '2____ (subst-one random-verb1 '3____ (subst-one random-noun1 '4____ '(This trio 1____ up a refreshing 2____ ritual - either 3____ hot 'or over 4____ )))))])))

(define green-tea-all (concat (green-tea-mad-lib 'green-tea-1) (green-tea-mad-lib 'green-tea-2)))

(define green-tea
  (lambda ()
    (display green-tea-all)))

;;History of Sonic the HedgeHog Mad Lib
(define sonic-mad-lib
  (lambda (line)
    (cond
      [(equal? 'sonic-1 line) (subst-one random-adjective1 '____ '(It`s 1990, and SEGA isn`t making very ____ games. ))]
      [(equal? 'sonic-2 line) '(Nintendo`s mascot, Mario, is making more and more games that are way better than any of SEGA`s.)]
      [(equal? 'sonic-3 line) (subst-one random-noun1 '1____ (subst-one random-noun2 '2____ (subst-one random-body-part-plural '3____ (subst-one random-verb1 '4____ '(Then SEGA had a/an 1____ ‘, what they needed is a Mario of their own...they had the 2____ of making a rabbit that can stretch his 3____ out and 4____ things. )))))]
      [(equal? 'sonic-4 line) '(That plan didn`t go so well.)]
      [(equal? 'sonic-5 line) (subst-one random-verb2 '1____ (subst-one random-verb3 '2____ '(They needed an animal that can 1____ itself and 2____ enemies by doing only a spin. )))]
      [(equal? 'sonic-6 line) (subst-one random-verb2 '1____ (subst-one random-verb3 '2____ '(They noticed that hedgehogs have quills that 1____ and 2____ )))]
      [(equal? 'sonic-7 line) (subst-one random-verb-ing1 '____ '(So they went with the hedgehog. Since he was planned to be a rabbit, they programmed him to run at a ____ speed. ))]
      [(equal? 'sonic-8 line) (subst-one random-noun3 '____ '(Then, in 1991, `Sonic the Hedgehog` was released for the SEGA ____))]
      [(equal? 'sonic-9 line) (subst-one random-verb-ed1 '1____ (subst-one random-integer '2____ '(In the first year, it out- 1____ Mario by 2____ copies.)))]
      [(equal? 'sonic-10 line) (subst-one random-noun4 '____ '(Sonic was then known as SEGA`s ____))]
      [(equal? 'sonic-11 line) (subst-one random-plural-noun1 '____ '(Over the years, Sonic has taken on many ____))]
      [(equal? 'sonic-12 line) (subst-one random-adjective2 '____ '(In 1998, he took on a ____ style in the game `Sonic Adventure.))]
      [(equal? 'sonic-13 line) (subst-one random-adjective3 '____ '(Today, Mario and Sonic are still ____ rivals.))]
      [(equal? 'sonic-14 line) (subst-one random-place '1____  (subst-one random-letter1 '2____ (subst-one random-letter2 '3____ '(Also, in Winter 2007, Mario and Sonic will compete in the 2008 1____ Olympics in `Mario and Sonic at the Olympic Games` for the Nintendo Wii and Nintendo 2____ 3____ ))))])))

(define sonic-pt1 (concat (sonic-mad-lib 'sonic-1) (concat (sonic-mad-lib 'sonic-2) (concat (sonic-mad-lib 'sonic-3) (concat (sonic-mad-lib 'sonic-4) (concat (sonic-mad-lib 'sonic-5) (concat (sonic-mad-lib 'sonic-6) (sonic-mad-lib 'sonic-7))))))))
(define sonic-pt2 (concat (sonic-mad-lib 'sonic-8) (concat (sonic-mad-lib 'sonic-9) (concat (sonic-mad-lib 'sonic-10) (concat (sonic-mad-lib 'sonic-11) (concat (sonic-mad-lib 'sonic-12) (concat (sonic-mad-lib 'sonic-13) (sonic-mad-lib 'sonic-14))))))))

(define sonic
  (lambda ()
    (display (concat sonic-pt1 sonic-pt2))))

;;Mad Lib main function
(define mad-lib
  (lambda (title)
    (cond
      ;;all mad-libs
      [(equal? 'best-friends-mad-lib title) (best-friends)]
      [(equal? 'sick-mad-lib title) (sick)]
      [(equal? 'store-mad-lib title) (store)]
      [(equal? 'pizza-mad-lib title) (pizza)]
      [(equal? 'pbj-mad-lib title) (pbj)]
      [(equal? 'green-tea-mad-lib title) (green-tea)]
      [(equal? 'sonic-mad-lib title) (sonic)]
      ;; Not actually a story, but a small easter egg in my code
      [(equal? 'hello-mad-lib title) '(Hello to you too! :D)]
      [else (error "Please enter a valid title")])))
