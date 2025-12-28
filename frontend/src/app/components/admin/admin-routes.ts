import { Routes } from '@angular/router';
import { AdminGuardFn } from '../../guards/admin.guard';
import { Admin } from './admin';

export const adminRoutes: Routes = [
  {
    path: 'admin',
    component: Admin,
    canActivate: [AdminGuardFn],
    children: [
      // ✅ Dashboard admin (khi bạn tạo AdminDashboard thì bỏ comment dòng dưới)
      {
        path: 'dashboard',
        loadComponent: () =>
          import('./admin-dashboard/admin-dashboard').then((m) => m.AdminDashboard),
        title: 'Dashboard Admin',
      },

      // ✅ Quản lý bộ câu hỏi (Admin)
      {
        path: 'bo-cau-hoi',
        loadComponent: () =>
          import('./bo-cau-hoi/admin-bo-cau-hoi-list/admin-bo-cau-hoi-list').then(
            (m) => m.AdminBoCauHoiList
          ),
        title: 'Quản lý Bộ câu hỏi',
      },
      {
        path: 'bo-cau-hoi/tao-moi',
        loadComponent: () =>
          import('./bo-cau-hoi/admin-bo-cau-hoi-create/admin-bo-cau-hoi-create').then(
            (m) => m.AdminBoCauHoiCreate
          ),
        title: 'Tạo mới Bộ câu hỏi',
      },
      {
        path: 'bo-cau-hoi/sua-bo-cau-hoi/:id',
        loadComponent: () =>
          import('./bo-cau-hoi/admin-bo-cau-hoi-edit/admin-bo-cau-hoi-edit').then(
            (m) => m.AdminBoCauHoiEdit
          ),
        title: 'Chỉnh sửa Bộ câu hỏi',
      },
      {
        path: 'bo-cau-hoi/:id',
        loadComponent: () =>
          import('./bo-cau-hoi/admin-bo-cau-hoi-detail/admin-bo-cau-hoi-detail').then(
            (m) => m.AdminBoCauHoiDetail
          ),
        title: 'Chi tiết Bộ câu hỏi',
      },
      {
        path: 'bo-cau-hoi/:id/cau-hoi/tao-moi',
        loadComponent: () =>
          import('./cau-hoi/admin-cau-hoi-create/admin-cau-hoi-create').then(
            (m) => m.AdminCauHoiCreate
          ),
        title: 'Tạo mới Câu hỏi',
      },
      {
        path: 'cau-hoi/:id/chinh-sua',
        loadComponent: () =>
          import('./cau-hoi/admin-cau-hoi-edit/admin-cau-hoi-edit').then((m) => m.AdminCauHoiEdit),
        title: 'Chỉnh sửa Câu hỏi',
      },

      {
        path: 'users',
        loadComponent: () => import('./user/user-list/user').then((m) => m.AdminUserList),
        title: 'Quản lý Người dùng',
      },
      {
        path: 'users/:id',
        loadComponent: () => import('./user/user-detail/user-detail').then((m) => m.UserDetail),
        title: 'Chi tiết người dùng (Admin)',
      },
      {
        path: 'tran-dau',
        loadComponent: () =>
          import('./tran-dau/admin-trandau-list/admin-trandau-list').then(
            (m) => m.AdminTranDauList
          ),
        title: 'Quản lý Trận đấu',
      },
      {
        path: 'tran-dau/history/:lichSuId',
        loadComponent: () =>
          import('./tran-dau/admin-trandau-detail/admin-trandau-detail').then(
            (m) => m.AdminTrandauDetail
          ),
        title: 'Chi tiết lịch sử trận đấu (Admin)',
      },

      // ✅ Quản lý khóa học
      {
        path: 'khoa-hoc',
        loadComponent: () =>
          import('./khoa-hoc/admin-khoa-hoc-list/admin-khoa-hoc-list').then(
            (m) => m.AdminKhoaHocList
          ),
        title: 'Quản lý khóa học',
      },
      {
        path: 'khoa-hoc/tao-moi',
        loadComponent: () =>
          import('./khoa-hoc/admin-khoa-hoc-create/admin-khoa-hoc-create').then(
            (m) => m.AdminKhoaHocCreate
          ),
        title: 'Tạo khóa học',
      },
      {
        path: 'khoa-hoc/sua/:id',
        loadComponent: () =>
          import('./khoa-hoc/admin-khoa-hoc-edit/admin-khoa-hoc-edit').then(
            (m) => m.AdminKhoaHocEdit
          ),
        title: 'Chỉnh sửa khóa học',
      },
      {
        path: 'khoa-hoc/:id',
        loadComponent: () =>
          import('./khoa-hoc/admin-khoa-hoc-detail/admin-khoa-hoc-detail').then(
            (m) => m.AdminKhoaHocDetail
          ),
        title: 'Chi tiết khóa học',
      },

      // ✅ Quản lý cộng đồng
      {
        path: 'community',
        loadComponent: () =>
          import('./community/admin-community/admin-community').then(
            (m) => m.AdminCommunityComponent
          ),
        title: 'Quản lý cộng đồng',
      },

      // {
      //   path: 'thong-ke',
      //   component: AdminThongKe
      // },

      // Redirect mặc định khi vào /admin
      {
        path: '',
        pathMatch: 'full',
        redirectTo: 'dashboard',
      },
    ],
  },
];
