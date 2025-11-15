import {Routes} from '@angular/router';
import {Home} from './components/home/home';
import {Register} from './components/register/register';
import {Login} from './components/login/login';
import {Profile} from './components/profile/profile';
import {AuthGuardFn} from './guards/auth.guard';
import {GuestGuardFn} from './guards/guest.guard';
import {MainLayout} from './layout/main-layout/main-layout';
import {BoCauHoiList} from './components/bo-cau-hoi/list/list';
import {PhongCho} from './components/TranDau/phong-cho/phong-cho';

export const routes: Routes = [
  {
    path: '',
    component: MainLayout,
    canActivate: [AuthGuardFn],
    children: [
      {path: '', redirectTo: 'home', pathMatch: 'full'},
      {path: 'home', component: Home, title: 'Trang chủ'},
      {path: 'profile', component: Profile, title: 'Thông tin cá nhân'},
      {path: 'bo-cau-hoi/list', component: BoCauHoiList, title: 'Danh sách bộ câu hỏi'},
      {
        path: 'bo-cau-hoi/create',
        loadComponent: () => import('./components/bo-cau-hoi/create/create').then(m => m.BoCauHoiCreate),
        title: 'Tạo bộ câu hỏi'
      },
      {
        path: 'bo-cau-hoi/detail/:id',
        loadComponent: () => import('./components/bo-cau-hoi/detail/detail').then(m => m.BoCauHoiDetail),
        title: 'Chi tiết bộ câu hỏi'
      },
      {
        path: 'bo-cau-hoi/:id/questions/new',
        loadComponent: () => import('./components/cau-hoi/create/create').then(m => m.CauHoiCreate),
        title: 'Tạo câu hỏi'
      },
      {
        path: 'bo-cau-hoi/:boId/cau-hoi/:id/edit',
        loadComponent: () => import('./components/cau-hoi/edit/edit').then(m => m.CauHoiEdit),
        title: 'Chỉnh sửa câu hỏi',
      },
      {
        path: 'bo-cau-hoi/edit/:id',
        loadComponent: () => import('./components/bo-cau-hoi/edit/edit').then(m => m.BoCauHoiEdit),
        title: 'Chỉnh sửa bộ câu hỏi',
      },
      {path: 'battle/pending', component: PhongCho, title: 'Phòng đang chờ'},
      {
        path: 'battle/room/:id',
        loadComponent: () => import('./components/TranDau/chi-tiet-phong/chi-tiet-phong').then(m => m.ChiTietPhong)
      },
    ]
  },
  {path: 'login', component: Login, canActivate: [GuestGuardFn], title: 'Đăng nhập'},
  {path: 'register', component: Register, canActivate: [GuestGuardFn], title: 'Đăng ký'},
  {path: '**', redirectTo: 'home'},
];
