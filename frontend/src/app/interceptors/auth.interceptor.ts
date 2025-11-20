import {HttpHandlerFn, HttpInterceptorFn, HttpRequest} from '@angular/common/http';
import {inject} from '@angular/core';
import {TokenService} from '../services/token.service';

export const authInterceptor: HttpInterceptorFn = (req: HttpRequest<any>, next: HttpHandlerFn) => {
  const tokenService = inject(TokenService);

  const token = tokenService.getAccessToken();
  const isTokenValid = token && !tokenService.isTokenExpired();

  // Danh sách endpoint công khai không cần JWT
  const publicUrls = [
    '/users/dang-nhap',
    '/users/dang-ky',
    '/users/verify-email',
    '/users/resend-verification'
  ];

  // Nếu là endpoint public => bỏ qua
  const isPublic = publicUrls.some(url => req.url.includes(url));
  if (isPublic) return next(req);

  // Nếu có token hợp lệ => gắn header Authorization
  if (isTokenValid) {
    const authReq = req.clone({
      setHeaders: {Authorization: `Bearer ${token}`}
    });
    return next(authReq);
  }

  // Nếu không có token => gửi request gốc (public hoặc bị redirect ở guard)
  return next(req);
};
