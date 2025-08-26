defmodule DmarcWeb.TenantLive.Show do
  use DmarcWeb, :live_view

  alias Dmarc.Tenants

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <.header>
        Tenant {@tenant.id}
        <:subtitle>This is a tenant record from your database.</:subtitle>
        <:actions>
          <.button navigate={~p"/tenants"}>
            <.icon name="hero-arrow-left" />
          </.button>
          <.button variant="primary" navigate={~p"/tenants/#{@tenant}/edit?return_to=show"}>
            <.icon name="hero-pencil-square" /> Edit tenant
          </.button>
        </:actions>
      </.header>

      <.list>
        <:item title="Name">{@tenant.name}</:item>
      </.list>
    </Layouts.app>
    """
  end

  @impl true
  def mount(%{"id" => id}, _session, socket) do
    if connected?(socket) do
      Tenants.subscribe_tenants(socket.assigns.current_scope)
    end

    {:ok,
     socket
     |> assign(:page_title, "Show Tenant")
     |> assign(:tenant, Tenants.get_tenant!(socket.assigns.current_scope, id))}
  end

  @impl true
  def handle_info(
        {:updated, %Dmarc.Tenants.Tenant{id: id} = tenant},
        %{assigns: %{tenant: %{id: id}}} = socket
      ) do
    {:noreply, assign(socket, :tenant, tenant)}
  end

  def handle_info(
        {:deleted, %Dmarc.Tenants.Tenant{id: id}},
        %{assigns: %{tenant: %{id: id}}} = socket
      ) do
    {:noreply,
     socket
     |> put_flash(:error, "The current tenant was deleted.")
     |> push_navigate(to: ~p"/tenants")}
  end

  def handle_info({type, %Dmarc.Tenants.Tenant{}}, socket)
      when type in [:created, :updated, :deleted] do
    {:noreply, socket}
  end
end
