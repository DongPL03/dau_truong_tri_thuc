import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import Swal from 'sweetalert2';
import { ChuDe } from '../../../../models/chude';
import { KhoaHoiResponse } from '../../../../responses/khoahoc/khoa-hoi-response';
import { PageResponse } from '../../../../responses/page-response';
import { ResponseObject } from '../../../../responses/response-object';
import { Base } from '../../../base/base';

@Component({
  selector: 'app-admin-khoa-hoc-list',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './admin-khoa-hoc-list.html',
  styleUrl: './admin-khoa-hoc-list.scss',
})
export class AdminKhoaHocList extends Base implements OnInit {
  loading = false;
  keyword = '';
  chuDeId = 0;
  trangThai = '';
  sortOrder = 'NEWEST';
  page = 0;
  limit = 10;
  totalPages = 0;

  items: KhoaHoiResponse[] = [];
  chuDes: ChuDe[] = [];

  readonly trangThaiOptions = [
    { value: '', label: 'Tất cả' },
    { value: 'PUBLISHED', label: 'Đã xuất bản' },
    { value: 'DRAFT', label: 'Bản nháp' },
    { value: 'ARCHIVED', label: 'Đã lưu trữ' },
  ];

  ngOnInit(): void {
    this.loadChuDe();
    this.loadData();
  }

  loadChuDe(): void {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<ChuDe[]>) => {
        this.chuDes = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error');
      },
    });
  }

  loadData(): void {
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

  applyFilter() {
    this.page = 0;
    this.loadData();
  }

  clearFilter() {
    this.keyword = '';
    this.chuDeId = 0;
    this.trangThai = '';
    this.sortOrder = 'NEWEST';
    this.page = 0;
    this.loadData();
  }

  getVisiblePages(): number[] {
    const visible: number[] = [];
    const maxVisible = 5;
    const total = this.totalPages;

    if (total <= maxVisible) {
      return Array.from({ length: total }, (_, i) => i);
    }

    const start = Math.max(0, this.page - 2);
    const end = Math.min(total - 1, this.page + 2);

    if (start > 0) visible.push(0);
    if (start > 1) visible.push(-1);

    for (let i = start; i <= end; i++) visible.push(i);

    if (end < total - 2) visible.push(-2);
    if (end < total - 1) visible.push(total - 1);

    return visible;
  }

  changePage(newPage: number) {
    if (newPage >= 0 && newPage < this.totalPages) {
      this.page = newPage;
      this.loadData();
    }
  }

  navigateToCreate() {
    this.router.navigate(['/admin/khoa-hoc/tao-moi']);
  }

  navigateToEdit(id: number) {
    this.router.navigate(['/admin/khoa-hoc/sua', id]);
  }

  navigateToDetail(id: number) {
    this.router.navigate(['/admin/khoa-hoc', id]);
  }

  onDelete(id: number) {
    Swal.fire({
      title: 'Xác nhận xóa khóa học?',
      html: `
        <div style="text-align: left; padding: 10px 0;">
          <p>Bạn chắc chắn muốn xóa mềm khóa học này?</p>
          <div style="background: #ffebee; padding: 15px; border-radius: 8px; margin-top: 10px; border-left: 4px solid #f44336;">
            <p style="margin: 0; color: #c62828; font-size: 0.9rem;">
              <i class="fas fa-info-circle"></i> Khóa học sẽ bị đánh dấu là đã xóa và không hiển thị cho người dùng.
            </p>
          </div>
        </div>
      `,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#f44336',
    }).then((result) => {
      if (result.isConfirmed) {
        this.khoaHocService.softDelete(id).subscribe({
          next: () => {
            Swal.fire('Thành công', 'Đã xóa khóa học thành công', 'success').then(() => {
              this.loadData();
            });
          },
          error: (err) => {
            const msg = err?.error?.message || 'Không thể xóa khóa học';
            Swal.fire('Lỗi', msg, 'error');
          },
        });
      }
    });
  }
}
