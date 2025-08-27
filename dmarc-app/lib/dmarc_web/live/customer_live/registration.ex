defmodule DmarcWeb.CustomerLive.Registration do
  use DmarcWeb, :live_view

  alias Dmarc.CustomerAccounts
  alias Dmarc.CustomerAccounts.Customer

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <div class="mx-auto max-w-sm">
        <div class="text-center">
          <.header>
            Register for an account
            <:subtitle>
              Already registered?
              <.link navigate={~p"/customers/log-in"} class="font-semibold text-brand hover:underline">
                Log in
              </.link>
              to your account now.
            </:subtitle>
          </.header>
        </div>

        <.form for={@form} id="registration_form" phx-submit="save" phx-change="validate">
          <.input
            field={@form[:email]}
            type="email"
            label="Email"
            autocomplete="username"
            required
            phx-mounted={JS.focus()}
          />
          <.input
            field={@form[:tenant_name]}
            type="text"
            label="Tenant"
            autocomplete="tenant"
            required
            phx-mounted={JS.focus()}
          />

          <.button phx-disable-with="Creating account..." class="btn btn-primary w-full">
            Create an account
          </.button>
        </.form>
      </div>
    </Layouts.app>
    """
  end

  @impl true
  def mount(_params, _session, %{assigns: %{current_scope: %{customer: customer}}} = socket)
      when not is_nil(customer) do
    {:ok, redirect(socket, to: DmarcWeb.CustomerAuth.signed_in_path(socket))}
  end

  def mount(_params, _session, socket) do
    changeset = CustomerAccounts.change_customer_email(%Customer{}, %{}, validate_unique: false)

    {:ok, assign_form(socket, changeset), temporary_assigns: [form: nil]}
  end

  @impl true
  def handle_event("save", %{"customer" => customer_params} = _opts, socket) do
    # IO.puts("=========================== save #{inspect opts}")
    case CustomerAccounts.register_customer(customer_params) do
      {:ok, customer} ->
        {:ok, _} =
          CustomerAccounts.deliver_login_instructions(
            customer,
            &url(~p"/customers/log-in/#{&1}")
          )

        {:noreply,
         socket
         |> put_flash(
           :info,
           "An email was sent to #{customer.email}, please access it to confirm your account."
         )
         |> push_navigate(to: ~p"/customers/log-in")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  def handle_event("validate", %{"customer" => customer_params}, socket) do
    changeset = CustomerAccounts.change_customer_email(%Customer{}, customer_params, validate_unique: false)
    {:noreply, assign_form(socket, Map.put(changeset, :action, :validate))}
  end

  defp assign_form(socket, %Ecto.Changeset{} = changeset) do
    form = to_form(changeset, as: "customer")
    assign(socket, form: form)
  end
end
