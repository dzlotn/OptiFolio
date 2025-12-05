(** Investment goal options *)
type investment_goal =
  | Growth        (** Capital appreciation focus *)
  | Income        (** Dividend/regular income focus *)
  | Balanced      (** Growth and income balance *)
  | Preservation  (** Capital preservation focus *)

(** Investment experience levels *)
type investment_experience =
  | Beginner      (** New to stock investing *)
  | Intermediate  (** Some experience with stocks *)
  | Experienced   (** Extensive stock investing experience *)

(** Risk tolerance levels *)
type risk_tolerance =
  | Conservative  (** Low risk, stable returns *)
  | Moderate      (** Balanced risk and return *)
  | Aggressive    (** High risk, higher potential returns *)

(** Investment time horizons *)
type time_horizon =
  | Short_term    (** 1-3 years *)
  | Medium_term   (** 3-7 years *)
  | Long_term     (** 7+ years *)

(** Portfolio size preferences *)
type portfolio_size =
  | Small   (** 3-5 stocks *)
  | Medium  (** 5-10 stocks *)
  | Large   (** 10+ stocks *)

(** Complete questionnaire responses *)
type questionnaire_responses = {
  name : string;                      (** User's name *)
  goal : investment_goal;             (** Primary investment goal *)
  experience : investment_experience; (** Investment experience level *)
  risk : risk_tolerance;              (** Risk tolerance *)
  horizon : time_horizon;             (** Investment time horizon *)
  portfolio_size : portfolio_size;    (** Desired portfolio size *)
  willing_to_lose : bool;            (** Willingness to accept temporary losses *)
  has_current_investments : bool;    (** Whether user has current investments *)
  current_investments : string list option; (** List of current investment tickers *)
}

(** [parse_goal input] parses investment goal from user input.
    Accepts "1", "growth" for Growth; "2", "income" for Income;
    "3", "balanced" for Balanced; "4", "preservation" for Preservation.
    Returns: [None] for invalid input. *)
val parse_goal : string -> investment_goal option

(** [parse_experience input] parses investment experience from user input.
    Accepts "1", "beginner" for Beginner; "2", "intermediate" for Intermediate;
    "3", "experienced" for Experienced.
    Returns: [None] for invalid input. *)
val parse_experience : string -> investment_experience option

(** [parse_risk input] parses risk tolerance from user input.
    Accepts "1", "conservative" for Conservative; "2", "moderate" for Moderate;
    "3", "aggressive" for Aggressive.
    Returns: [None] for invalid input. *)
val parse_risk : string -> risk_tolerance option

(** [parse_horizon input] parses time horizon from user input.
    Accepts "1", "short-term", "short" for Short_term;
    "2", "medium-term", "medium" for Medium_term;
    "3", "long-term", "long" for Long_term.
    Returns: [None] for invalid input. *)
val parse_horizon : string -> time_horizon option

(** [parse_portfolio_size input] parses portfolio size from user input.
    Accepts "1", "small" for Small; "2", "medium" for Medium;
    "3", "large" for Large.
    Returns: [None] for invalid input. *)
val parse_portfolio_size : string -> portfolio_size option

(** [parse_yes_no input] parses yes/no responses from user input.
    Accepts "yes", "y", "true", "1" for true;
    "no", "n", "false", "2" for false.
    Returns: [None] for invalid input. *)
val parse_yes_no : string -> bool option

