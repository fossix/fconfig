;;; TODO a lot to be done
(defun future-value (amount rate years)
  (* amount (expt (+ 1 (/ (float rate) 100)) years)))

(defun calculate-compounded-value (amount rate tenure-start tenure-end)
  "Calculate the total compounded value of AMOUNT compounded at a
RATE for TENURE-START to TENURE-END years. This can be used to
calculate future value of an investment or to get how much you
need when you retire, considering inflation.

If you want to calculate how much your monthly invest of 10000
will be after 10 years with a interest rate of 8 percent

    (calculate-compounded-value (* 12 10000) 8 0 10)

TENURE-START is 0 before we are calculating from this year.

Again if a retirement fund needed is to be calculated, the RATE
will be the inflation rate, and tenure start will be \"number of
years from today you want to retire\", and tenure end is number
years from today that one is expected to live. So a monthly need
of 20000 for 20 years after retiring around 25 years from now,
with inflation of 4%

    (calculate-compounded-value (* 12 20000) 4 25 20)
"

  (let ((total 0)
        (x 0))
    (while (< x (- tenure-end tenure-start))
      (setq total (+ total (future-value amount rate (+ tenure-start x))))
      (incf x))
    total))
