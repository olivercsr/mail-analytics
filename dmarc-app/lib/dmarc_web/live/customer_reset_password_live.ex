defmodule DmarcWeb.CustomerResetPasswordLive do
  use DmarcWeb, :live_view

  alias Dmarc.CustomerAccounts

  def render(assigns) do
    ~H"""
    <div class="mx-auto max-w-sm">
      <.header class="text-center">Reset Password</.header>

      <.simple_form
        for={@form}
        id="reset_password_form"
        phx-submit="reset_password"
        phx-change="validate"
      >
        <.error :if={@form.errors != []}>
          Oops, something went wrong! Please check the errors below.
        </.error>

        <.input field={@form[:password]} type="password" label="New password" required />
        <.input
          field={@form[:password_confirmation]}
          type="password"
          label="Confirm new password"
          required
        />
        <:actions>
          <.button phx-disable-with="Resetting..." class="w-full">Reset Password</.button>
        </:actions>
      </.simple_form>

      <p class="text-center text-sm mt-4">
        <.link href={~p"/customers/register"}>Register</.link>
        | <.link href={~p"/customers/log_in"}>Log in</.link>
      </p>
    </div>
    """
  end

  def mount(params, _session, socket) do
    socket = assign_customer_and_token(socket, params)

    form_source =
      case socket.assigns do
        %{customer: customer} ->
          CustomerAccounts.change_customer_password(customer)

        _ ->
          %{}
      end

    {:ok, assign_form(socket, form_source), temporary_assigns: [form: nil]}
  end

  # Do not log in the customer after reset password to avoid a
  # leaked token giving the customer access to the account.
  def handle_event("reset_password", %{"customer" => customer_params}, socket) do
    case CustomerAccounts.reset_customer_password(socket.assigns.customer, customer_params) do
      {:ok, _} ->
        {:noreply,
         socket
         |> put_flash(:info, "Password reset successfully.")
         |> redirect(to: ~p"/customers/log_in")}

      {:error, changeset} ->
        {:noreply, assign_form(socket, Map.put(changeset, :action, :insert))}
    end
  end

  def handle_event("validate", %{"customer" => customer_params}, socket) do
    changeset = CustomerAccounts.change_customer_password(socket.assigns.customer, customer_params)
    {:noreply, assign_form(socket, Map.put(changeset, :action, :validate))}
  end

  defp assign_customer_and_token(socket, %{"token" => token}) do
    if customer = CustomerAccounts.get_customer_by_reset_password_token(token) do
      assign(socket, customer: customer, token: token)
    else
      socket
      |> put_flash(:error, "Reset password link is invalid or it has expired.")
      |> redirect(to: ~p"/")
    end
  end

  defp assign_form(socket, %{} = source) do
    assign(socket, :form, to_form(source, as: "customer"))
  end
end
