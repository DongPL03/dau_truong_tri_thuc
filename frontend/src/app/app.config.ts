import {ApplicationConfig, provideZoneChangeDetection} from '@angular/core';
import {provideRouter} from '@angular/router';

import {routes} from './app.routes';
import {provideClientHydration} from '@angular/platform-browser';
import {provideHttpClient, withFetch, withInterceptors} from '@angular/common/http';
import {provideToastr} from 'ngx-toastr'
import {provideAnimations} from '@angular/platform-browser/animations';
import {tokenInterceptor} from './interceptors/token.interceptor';

export const appConfig: ApplicationConfig = {
  providers: [
    // provideBrowserGlobalErrorListeners(),
    // provideZoneChangeDetection({ eventCoalescing: true }),
    // provideRouter(routes), provideClientHydration(withEventReplay())
    provideZoneChangeDetection({ eventCoalescing: true }),
    provideRouter(routes),
    provideClientHydration(),
    provideHttpClient(
      withInterceptors([tokenInterceptor]), withFetch()
    ),
    provideToastr(),
    provideAnimations()
    // provideHttpClient(withInterceptors([authInterceptor, errorInterceptor])),
  ]
};
