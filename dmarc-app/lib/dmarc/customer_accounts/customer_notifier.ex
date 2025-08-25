defmodule Dmarc.CustomerAccounts.CustomerNotifier do
  import Swoosh.Email

  alias Dmarc.Mailer

  # Delivers the email using the application mailer.
  defp deliver(recipient, subject, body) do
    email =
      new()
      |> to(recipient)
      |> from({"Dmarc", "contact@example.com"})
      |> subject(subject)
      |> text_body(body)

    with {:ok, _metadata} <- Mailer.deliver(email) do
      {:ok, email}
    end
  end

  @doc """
  Deliver instructions to confirm account.
  """
  def deliver_confirmation_instructions(customer, url) do
    deliver(customer.email, "Confirmation instructions", """

    ==============================

    Hi #{customer.email},

    You can confirm your account by visiting the URL below:

    #{url}

    If you didn't create an account with us, please ignore this.

    ==============================
    """)
  end

  @doc """
  Deliver instructions to reset a customer password.
  """
  def deliver_reset_password_instructions(customer, url) do
    deliver(customer.email, "Reset password instructions", """

    ==============================

    Hi #{customer.email},

    You can reset your password by visiting the URL below:

    #{url}

    If you didn't request this change, please ignore this.

    ==============================
    """)
  end

  @doc """
  Deliver instructions to update a customer email.
  """
  def deliver_update_email_instructions(customer, url) do
    deliver(customer.email, "Update email instructions", """

    ==============================

    Hi #{customer.email},

    You can change your email by visiting the URL below:

    #{url}

    If you didn't request this change, please ignore this.

    ==============================
    """)
  end
end
