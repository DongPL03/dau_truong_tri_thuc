import { Routes } from '@angular/router';
import { Admin } from './components/admin/admin';
import { BoCauHoiList } from './components/bo-cau-hoi/danh-sach-bo-cau-hoi/danh-sach-bo-cau-hoi';
import { DangKy } from './components/dang-ky/dang-ky';
import { DangNhap } from './components/dang-nhap/dang-nhap';
import { Home } from './components/home/home';
import { Profile } from './components/profile/profile';
import { PhongCho } from './components/TranDau/phong-cho/phong-cho';
import { AdminGuardFn } from './guards/admin.guard';
import { AuthGuardFn } from './guards/auth.guard';
import { GuestGuardFn } from './guards/guest.guard';
import { MainLayout } from './layout/main-layout/main-layout';

export const routes: Routes = [
  {
    path: '',
    component: MainLayout,
    canActivate: [AuthGuardFn],
    children: [
      { path: '', redirectTo: 'home', pathMatch: 'full' },
      { path: 'home', component: Home, title: 'Trang chủ' },
      { path: 'profile', component: Profile, title: 'Thông tin cá nhân' },
      {
        path: 'bo-cau-hoi/danh-sach-bo-cau-hoi',
        component: BoCauHoiList,
        title: 'Danh sách bộ câu hỏi',
      },
      {
        path: 'bo-cau-hoi/tao-moi-bo-cau-hoi',
        loadComponent: () =>
          import('./components/bo-cau-hoi/tao-moi-bo-cau-hoi/tao-moi-bo-cau-hoi').then(
            (m) => m.BoCauHoiCreate
          ),
        title: 'Tạo bộ câu hỏi',
      },
      {
        path: 'bo-cau-hoi/chi-tiet-bo-cau-hoi/:id',
        loadComponent: () =>
          import('./components/bo-cau-hoi/chi-tiet-bo-cau-hoi/chi-tiet-bo-cau-hoi').then(
            (m) => m.BoCauHoiDetail
          ),
        title: 'Chi tiết bộ câu hỏi',
      },
      {
        path: 'bo-cau-hoi/:id/cau-hoi/tao-moi-cau-hoi',
        loadComponent: () =>
          import('./components/cau-hoi/tao-moi-cau-hoi/tao-moi-cau-hoi').then(
            (m) => m.CauHoiCreate
          ),
        title: 'Tạo câu hỏi',
      },
      {
        path: 'bo-cau-hoi/:boId/cau-hoi/:id/sua-bo-cau-hoi',
        loadComponent: () =>
          import('./components/cau-hoi/sua-cau-hoi/sua-cau-hoi').then((m) => m.CauHoiEdit),
        title: 'Chỉnh sửa câu hỏi',
      },
      {
        path: 'bo-cau-hoi/sua-bo-cau-hoi/:id',
        loadComponent: () =>
          import('./components/bo-cau-hoi/sua-bo-cau-hoi/sua-bo-cau-hoi').then(
            (m) => m.BoCauHoiEdit
          ),
        title: 'Chỉnh sửa bộ câu hỏi',
      },
      { path: 'tran-dau/pending', component: PhongCho, title: 'Phòng đang chờ' },
      {
        path: 'tran-dau/phong/:id',
        loadComponent: () =>
          import('./components/TranDau/chi-tiet-phong/chi-tiet-phong').then((m) => m.ChiTietPhong),
      },
      {
        path: 'tran-dau/tao-moi-bo-cau-hoi',
        loadComponent: () =>
          import('./components/TranDau/tao-tran/tao-tran').then((m) => m.TaoTran),
        title: 'Tạo trận đấu',
      },
      {
        path: 'tran-dau/lich-su-tran-dau',
        loadComponent: () =>
          import('./components/TranDau/lich-su-tran-dau/lich-su-tran-dau').then(
            (m) => m.LichSuTranDau
          ),
        title: 'Lịch sử trận đấu',
      },
      {
        path: 'tran-dau/lich-su-tran-dau/:id',
        loadComponent: () =>
          import('./components/TranDau/lich-su-tran-dau-detail/lich-su-tran-dau-detail').then(
            (m) => m.LichSuTranDauDetail
          ),
        title: 'Chi tiết lịch sử trận đấu',
      },
      {
        path: 'luyen-tap',
        loadComponent: () =>
          import('./components/luyen-tap/luyen-tap-home/luyen-tap-home').then(
            (m) => m.LuyenTapHomeComponent
          ),
        title: 'Luyện tập cá nhân',
      },
      {
        path: 'goi-y-hoc-tap',
        loadComponent: () =>
          import('./components/recommendation/goi-y-hoc-tap/goi-y-hoc-tap').then(
            (m) => m.GoiYHocTapComponent
          ),
        title: 'Gợi ý học tập',
      },
      {
        path: 'bang-xep-hang',
        loadComponent: () =>
          import('./components/bang-xep-hang/bang-xep-hang').then((m) => m.BangXepHang),
        title: 'Bảng xếp hạng',
      },
      {
        path: 'nguoi-choi/:id',
        loadComponent: () =>
          import('./components/nguoi-choi/ho-so-nguoi-choi/ho-so-nguoi-choi').then(
            (m) => m.HoSoNguoiChoi
          ),
        title: 'Hồ sơ người chơi',
      },
      {
        path: 'ban-be',
        loadComponent: () =>
          import('./components/ban-be/user-friend/user-friend').then((m) => m.UserFriend),
        title: 'Bạn bè',
      },
      {
        path: 'danh-sach-thong-bao',
        loadComponent: () =>
          import(
            './components/notification-page/user-notification-page/user-notification-page'
          ).then((m) => m.UserNotificationPage),
        title: 'Thông báo',
      },
      {
        path: 'chat',
        loadComponent: () =>
          import('./components/chat/user-chat/user-chat').then((m) => m.UserChat),
        title: 'Trò chuyện',
      },
      {
        path: 'chat/:partner_id',
        loadComponent: () =>
          import('./components/chat/user-chat/user-chat').then((m) => m.UserChat),
        title: 'Trò chuyện',
      },
      {
        path: 'khoa-hoc',
        loadComponent: () =>
          import('./components/khoa-hoc/danh-sach-khoa-hoc/danh-sach-khoa-hoc').then(
            (m) => m.DanhSachKhoaHoc
          ),
        title: 'Khóa học',
      },
      {
        path: 'khoa-hoc/:id',
        loadComponent: () =>
          import('./components/khoa-hoc/chi-tiet-khoa-hoc/chi-tiet-khoa-hoc').then(
            (m) => m.ChiTietKhoaHoc
          ),
        title: 'Chi tiết khóa học',
      },
      {
        path: 'khoa-hoc/:khoaHocId/luyen-tap/:boCauHoiId',
        loadComponent: () =>
          import('./components/khoa-hoc/luyen-tap-khoa-hoc/luyen-tap-khoa-hoc').then(
            (m) => m.LuyenTapKhoaHocComponent
          ),
        title: 'Luyện tập khóa học',
      },
      {
        path: 'khoa-hoc/:khoaHocId/luyen-tap/:boCauHoiId/memo',
        loadComponent: () =>
          import('./components/khoa-hoc/luyen-tap-khoa-hoc/luyen-tap-khoa-hoc').then(
            (m) => m.LuyenTapKhoaHocComponent
          ),
        title: 'Luyện tập lại câu sai',
      },
    ],
  },
  {
    path: 'admin',
    component: Admin,
    canActivate: [AdminGuardFn],
  },

  { path: 'login', component: DangNhap, canActivate: [GuestGuardFn], title: 'Đăng nhập' },
  { path: 'register', component: DangKy, canActivate: [GuestGuardFn], title: 'Đăng ký' },
  { path: '**', redirectTo: 'home' },
];
