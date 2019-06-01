#region Copyright Syncfusion Inc. 2001-2018.
// Copyright Syncfusion Inc. 2001-2018. All rights reserved.
// Use of this code is subject to the terms of our license.
// A copy of the current license can be obtained at any time by e-mailing
// licensing@syncfusion.com. Any infringement will be prosecuted under
// applicable laws. 
#endregion
using Microsoft.Owin;
using Owin;
using Microsoft.AspNet.Identity;
using Microsoft.AspNet.Identity.EntityFramework;
using syncfusion_payc.Models;

using System.Security.Claims;

[assembly: OwinStartupAttribute(typeof(syncfusion_payc.Startup))]
namespace syncfusion_payc
{
    public partial class Startup
    {
        public void Configuration(IAppBuilder app)
        {
            ConfigureAuth(app);
            createRolesandUsers();
        }
        private void createRolesandUsers()
        {
            ApplicationDbContext context = new ApplicationDbContext();

            var roleManager = new RoleManager<IdentityRole>(new RoleStore<IdentityRole>(context));
            var UserManager = new UserManager<ApplicationUser>(new UserStore<ApplicationUser>(context));

            // Se crea el usuario y rol de pruebas para mauricio
            if (!roleManager.RoleExists("Pruebas"))
            {

                // Se crea el rol Sistemas
                var role = new Microsoft.AspNet.Identity.EntityFramework.IdentityRole();
                role.Name = "Pruebas";
                roleManager.Create(role);

                //Se crea el usuario SISTEMAS                  

                var user = new ApplicationUser();
                user.UserName = "PRUEBAS";
                user.Email = "pruebas@payc.com.co";

                string userPWD = "Pruebas2019*";

                var chkUser = UserManager.Create(user, userPWD);

                //Crear Usuario Sistemas 
                if (chkUser.Succeeded)
                {
                    var result1 = UserManager.AddToRole(user.Id, "Pruebas");

                }
            }


            // Se crea el usuario y rol sistemas
            if (!roleManager.RoleExists("Sistemas"))
            {

                // Se crea el rol Sistemas
                var role = new Microsoft.AspNet.Identity.EntityFramework.IdentityRole();
                role.Name = "Sistemas";
                roleManager.Create(role);

                //Se crea el usuario SISTEMAS                  

                var user = new ApplicationUser();
                user.UserName = "SISTEMAS";
                user.Email = "sistemas@payc.com.co";

                string userPWD = "SISTEMAS2015*";

                var chkUser = UserManager.Create(user, userPWD);

                //Crear Usuario Sistemas 
                if (chkUser.Succeeded)
                {
                    var result1 = UserManager.AddToRole(user.Id, "Sistemas");

                }
            }




            // Se crea el usuario y rol admin
            if (!roleManager.RoleExists("Admin"))
            {

                // Se crea el rol admin
                var role = new Microsoft.AspNet.Identity.EntityFramework.IdentityRole();
                role.Name = "Admin";
                roleManager.Create(role);

                //Se crea el usuario administrador                  

                var user = new ApplicationUser();
                user.UserName = "ADMIN";
                user.Email = "director.analitica@payc.com.co";

                string userPWD = "1234JAMS*";

                var chkUser = UserManager.Create(user, userPWD);

                //Crear Usuario Admin 
                if (chkUser.Succeeded)
                {
                    var result1 = UserManager.AddToRole(user.Id, "Admin");

                }
            }

            //  Crear rol calificador 
            if (!roleManager.RoleExists("Calificador"))
            {
                var role = new Microsoft.AspNet.Identity.EntityFramework.IdentityRole();
                role.Name = "Calificador";
                roleManager.Create(role);

            }

            // Crear rol revisor calificaci�n
            if (!roleManager.RoleExists("Revisor_Calificaciones"))
            {
                var role = new Microsoft.AspNet.Identity.EntityFramework.IdentityRole();
                role.Name = "Revisor_Calificaciones";
                roleManager.Create(role);

            }
            // Crear rol usuario externo (para el registro externo)
            if (!roleManager.RoleExists("Usuario_Externo"))
            {
                var role = new Microsoft.AspNet.Identity.EntityFramework.IdentityRole();
                role.Name = "Usuario_Externo";
                roleManager.Create(role);

            }
            // Crear rol Directivo
            if (!roleManager.RoleExists("Directivo"))
            {
                var role = new Microsoft.AspNet.Identity.EntityFramework.IdentityRole();
                role.Name = "Directivo";
                roleManager.Create(role);

            }


        }
    }
}
