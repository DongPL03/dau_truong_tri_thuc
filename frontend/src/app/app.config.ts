import {ApplicationConfig, importProvidersFrom, provideZoneChangeDetection} from '@angular/core';
import {provideRouter, RouterModule} from '@angular/router';

import {routes} from './app.routes';
import {provideClientHydration} from '@angular/platform-browser';
import {provideHttpClient, withFetch, withInterceptors} from '@angular/common/http';
import {provideToastr} from 'ngx-toastr'
import {provideAnimations} from '@angular/platform-browser/animations';
import {tokenInterceptor} from './interceptors/token.interceptor';
import {adminRoutes} from './components/admin/admin-routes';
import {provideCharts, withDefaultRegisterables} from 'ng2-charts';

export const appConfig: ApplicationConfig = {
  providers: [
    // provideBrowserGlobalErrorListeners(),
    // provideZoneChangeDetection({ eventCoalescing: true }),
    // provideRouter(routes), provideClientHydration(withEventReplay())
    importProvidersFrom(RouterModule.forChild(adminRoutes)),
    provideZoneChangeDetection({eventCoalescing: true}),
    provideRouter(routes),
    provideClientHydration(),
    provideHttpClient(
      withInterceptors([tokenInterceptor]), withFetch()
    ),
    provideToastr(),
    provideAnimations(),
    provideCharts(withDefaultRegisterables()) // Cấu hình cho Chart.js
    // provideHttpClient(withInterceptors([authInterceptor, errorInterceptor])),
  ]
};
