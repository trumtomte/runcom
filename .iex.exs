Application.put_env(:elixir, :ansi_enabled, true)

IEx.configure(
  colors: [
    eval_result: [:green],
    eval_error: [:red],
    eval_info: [:yellow]
  ],
  history_size: 50,
  inspect: [
    pretty: true, 
    limit: :infinity,
    width: 80
  ],
  width: 80,
  default_prompt: [
    "\e[G",
    :white,
    "[",
    :blue,
    "%prefix",
    ": ",
    "%counter",
    :white,
    "]:",
    :reset
  ] |> IO.ANSI.format |> IO.chardata_to_string
)
