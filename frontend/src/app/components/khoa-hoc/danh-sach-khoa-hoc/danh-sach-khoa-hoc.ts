import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import Swal from 'sweetalert2';
import { ChuDe } from '../../../models/chude';
import { KhoaHoiResponse } from '../../../responses/khoahoc/khoa-hoi-response';
import { PageResponse } from '../../../responses/page-response';
import { ResponseObject } from '../../../responses/response-object';
import { Base } from '../../base/base';

@Component({
  selector: 'app-danh-sach-khoa-hoc',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './danh-sach-khoa-hoc.html',
  styleUrl: './danh-sach-khoa-hoc.scss',
})
export class DanhSachKhoaHoc extends Base implements OnInit {
  loading = false;
  keyword = '';
  trangThai = '';
  chuDeId = 0;
  page = 0;
  limit = 6;
  sortOrder = 'NEWEST';
  totalPages = 0;
  currentUserId: number = 0;
  items: KhoaHoiResponse[] = [];

  chuDes: ChuDe[] = [];
  readonly trangThaiOptions = [
    { value: '', label: 'Tất cả trạng thái' },
    { value: 'PUBLISHED', label: 'Đã xuất bản' },
    { value: 'DRAFT', label: 'Bản nháp' },
    { value: 'ARCHIVED', label: 'Đã lưu trữ' },
  ];

  ngOnInit() {
    this.currentUserId = this.tokenService.getUserId();
    this.loadData();
    this.loadChuDe();
  }

  loadData() {
    this.loading = true;
    this.khoaHocService
      .getAll(this.keyword, this.chuDeId, this.trangThai, this.sortOrder, this.page, this.limit)
      .subscribe({
        next: (res: ResponseObject<PageResponse<KhoaHoiResponse>>) => {
          const data = res.data!;
          this.items = data.items ?? [];
          this.totalPages = data.totalPages;
          this.loading = false;
        },
        error: () => {
          this.loading = false;
          Swal.fire('Lỗi', 'Không thể tải danh sách khóa học', 'error');
        },
      });
  }

  loadChuDe() {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<any>) => {
        this.chuDes = res.data || [];
      },
      error: () => {
        console.error('Không thể tải danh sách chủ đề');
      },
    });
  }

  applyFilter() {
    this.page = 0;
    this.loadData();
  }

  getVisiblePages(): number[] {
    const visible: number[] = [];
    const maxVisible = 5;
    const total = this.totalPages;

    if (total <= maxVisible) {
      return Array.from({length: total}, (_, i) => i);
    }

    const start = Math.max(0, this.page - 2);
    const end = Math.min(total - 1, this.page + 2);

    if (start > 0) visible.push(0);
    if (start > 1) visible.push(-1); // -1 là dấu ...

    for (let i = start; i <= end; i++) visible.push(i);

    if (end < total - 2) visible.push(-2); // -2 là dấu ...
    if (end < total - 1) visible.push(total - 1);

    return visible;
  }

  changePage(newPage: number) {
    if (newPage >= 0 && newPage < this.totalPages) {
      this.page = newPage;
      this.loadData();
    }
  }

  navigateToDetail(id: number) {
    this.router.navigate(['/khoa-hoc', id]);
  }

  navigateToCreate() {
    this.router.navigate(['/khoa-hoc/tao-moi']);
  }

  navigateToEdit(id: number) {
    this.router.navigate(['/khoa-hoc', id, 'sua']);
  }

  isOwner(khoaHoc: KhoaHoiResponse): boolean {
    return khoaHoc.nguoi_tao_id === this.currentUserId;
  }

  isAdmin(): boolean {
    return this.tokenService.getRoles().includes('ROLE_ADMIN');
  }

  canEditOrDelete(khoaHoc: KhoaHoiResponse): boolean {
    return this.isAdmin() || this.isOwner(khoaHoc);
  }

  onDelete(id: number) {
    Swal.fire({
      title: 'Xác nhận xóa?',
      text: 'Bạn có chắc muốn xóa khóa học này không?',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (result.isConfirmed) {
        this.khoaHocService.delete(id).subscribe({
          next: () => {
            Swal.fire('Thành công', 'Xóa khóa học thành công', 'success').then(() => {
              this.loadData();
            });
          },
          error: () => Swal.fire('Lỗi', 'Không thể xóa khóa học', 'error'),
        });
      }
    });
  }
}
