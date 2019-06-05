using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using System.Web.Routing;

namespace syncfusion_payc
{
	public class RouteConfig
	{
		public static void RegisterRoutes(RouteCollection routes)
		{
			routes.IgnoreRoute("{resource}.axd/{*pathInfo}");

            routes.IgnoreRoute("servicios/{resource}.svc/{*pathInfo}");
            routes.IgnoreRoute("servicios/{resource}.svc");
            routes.IgnoreRoute("Flujogramas_files/{*pathInfo}");
            routes.IgnoreRoute("Atencion_v1/{*pathInfo}");
            routes.MapRoute(
				name: "Default",
				url: "{controller}/{action}/{id}",
				defaults: new { controller = "Home", action = "Index", id = UrlParameter.Optional }
			);
		}
	}
}
