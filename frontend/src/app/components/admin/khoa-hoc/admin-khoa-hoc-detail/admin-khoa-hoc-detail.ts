import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import Swal from 'sweetalert2';
import { BoCauHoiResponse } from '../../../../responses/bocauhoi/bocauhoi-response';
import { BoCauHoiTrongKhoaResponse } from '../../../../responses/khoahoc/bo-cau-hoi-trong-khoa-response';
import { KhoaHoiDetailResponse } from '../../../../responses/khoahoc/khoa-hoi-detail-response';
import { PageResponse } from '../../../../responses/page-response';
import { ResponseObject } from '../../../../responses/response-object';
import { Base } from '../../../base/base';

@Component({
  selector: 'app-admin-khoa-hoc-detail',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './admin-khoa-hoc-detail.html',
  styleUrl: './admin-khoa-hoc-detail.scss',
})
export class AdminKhoaHocDetail extends Base implements OnInit {
  id!: number;
  detail?: KhoaHoiDetailResponse | null;
  loading = false;

  // Modal thêm bộ câu hỏi
  showAddModal = false;
  availableBoCauHoi: BoCauHoiResponse[] = [];
  loadingBoCauHoi = false;
  addForm = {
    bo_cau_hoi_id: 0,
    thu_tu: 1,
    is_bat_buoc: true,
    diem_toi_thieu: 0,
  };

  // Modal sửa bộ câu hỏi
  showEditModal = false;
  editingItem: BoCauHoiTrongKhoaResponse | null = null;
  editForm = {
    thu_tu: 1,
    is_bat_buoc: true,
    diem_toi_thieu: 0,
  };

  ngOnInit(): void {
    this.id = Number(this.route.snapshot.paramMap.get('id'));
    this.loadDetail();
  }

  loadDetail() {
    this.loading = true;
    this.khoaHocService.getById(this.id).subscribe({
      next: (res: ResponseObject<KhoaHoiDetailResponse>) => {
        this.detail = res.data;
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải chi tiết khóa học', 'error');
      },
    });
  }

  back() {
    this.router.navigate(['/admin/khoa-hoc']);
  }

  navigateToEdit() {
    this.router.navigate(['/admin/khoa-hoc/sua', this.id]);
  }

  get boCauHoiList(): BoCauHoiTrongKhoaResponse[] {
    return this.detail?.danh_sach_bo_cau_hoi || [];
  }

  openAddModal(): void {
    this.showAddModal = true;
    this.addForm = {
      bo_cau_hoi_id: 0,
      thu_tu:
        this.boCauHoiList.length > 0
          ? Math.max(...this.boCauHoiList.map((b) => b.thu_tu || 0)) + 1
          : 1,
      is_bat_buoc: true,
      diem_toi_thieu: 0,
    };
    this.loadAvailableBoCauHoi();
  }

  closeAddModal(): void {
    this.showAddModal = false;
    this.addForm = {
      bo_cau_hoi_id: 0,
      thu_tu: 1,
      is_bat_buoc: true,
      diem_toi_thieu: 0,
    };
  }

  loadAvailableBoCauHoi(): void {
    this.loadingBoCauHoi = true;
    // Lấy danh sách bộ câu hỏi đã duyệt, chỉ lấy COURSE_ONLY và do admin tạo
    this.bocauHoiService
      .getAll('', 0, '', 'DA_DUYET', 'COURSE_ONLY', undefined, 0, 'NEWEST', 0, 100)
      .subscribe({
        next: (res: ResponseObject<PageResponse<BoCauHoiResponse>>) => {
          const allBoCauHoi = res.data?.items || [];
          // Lọc ra những bộ câu hỏi:
          // 1. Chưa có trong khóa học này
          // 2. Có loai_su_dung = COURSE_ONLY
          // 3. Do admin tạo (backend đã filter, nhưng kiểm tra lại để chắc chắn)
          const existingIds = new Set(this.boCauHoiList.map((b) => b.bo_cau_hoi_id));
          this.availableBoCauHoi = allBoCauHoi.filter(
            (b) => !existingIds.has(b.id) && b.loai_su_dung === 'COURSE_ONLY'
          );
          this.loadingBoCauHoi = false;
        },
        error: () => {
          this.loadingBoCauHoi = false;
          Swal.fire('Lỗi', 'Không thể tải danh sách bộ câu hỏi', 'error');
        },
      });
  }

  submitAdd(): void {
    if (!this.addForm.bo_cau_hoi_id || this.addForm.thu_tu < 1) {
      Swal.fire('Cảnh báo', 'Vui lòng chọn bộ câu hỏi và nhập thứ tự hợp lệ', 'warning');
      return;
    }

    this.khoaHocService.addBoCauHoiToKhoaHoc(this.id, this.addForm).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Đã thêm bộ câu hỏi vào khóa học', 'success');
        this.closeAddModal();
        this.loadDetail();
      },
      error: (err) => {
        const msg = err?.error?.message || 'Không thể thêm bộ câu hỏi';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }

  openEditModal(item: BoCauHoiTrongKhoaResponse): void {
    this.editingItem = item;
    this.editForm = {
      thu_tu: item.thu_tu || 1,
      is_bat_buoc: item.is_bat_buoc !== false,
      diem_toi_thieu: item.diem_toi_thieu || 0,
    };
    this.showEditModal = true;
  }

  closeEditModal(): void {
    this.showEditModal = false;
    this.editingItem = null;
    this.editForm = {
      thu_tu: 1,
      is_bat_buoc: true,
      diem_toi_thieu: 0,
    };
  }

  submitEdit(): void {
    if (!this.editingItem || this.editForm.thu_tu < 1) {
      Swal.fire('Cảnh báo', 'Thứ tự không hợp lệ', 'warning');
      return;
    }

    this.khoaHocService
      .updateBoCauHoiInKhoaHoc(this.id, this.editingItem.bo_cau_hoi_id!, this.editForm)
      .subscribe({
        next: () => {
          Swal.fire('Thành công', 'Đã cập nhật bộ câu hỏi', 'success');
          this.closeEditModal();
          this.loadDetail();
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể cập nhật bộ câu hỏi';
          Swal.fire('Lỗi', msg, 'error');
        },
      });
  }

  removeBoCauHoi(item: BoCauHoiTrongKhoaResponse): void {
    Swal.fire({
      title: 'Xác nhận xóa?',
      text: `Bạn có chắc muốn xóa bộ câu hỏi "${item.tieu_de}" khỏi khóa học?`,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#f44336',
    }).then((result) => {
      if (!result.isConfirmed || !item.bo_cau_hoi_id) return;

      this.khoaHocService.removeBoCauHoiFromKhoaHoc(this.id, item.bo_cau_hoi_id).subscribe({
        next: () => {
          Swal.fire('Thành công', 'Đã xóa bộ câu hỏi khỏi khóa học', 'success');
          this.loadDetail();
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể xóa bộ câu hỏi';
          Swal.fire('Lỗi', msg, 'error');
        },
      });
    });
  }

  deleteKhoaHoc(): void {
    Swal.fire({
      title: 'Xác nhận xóa khóa học?',
      html: `
        <div style="text-align: left; padding: 10px 0;">
          <p><strong>Khóa học:</strong> ${this.detail?.khoa_hoc.tieu_de}</p>
          <p><strong>Số bộ câu hỏi:</strong> ${this.boCauHoiList.length}</p>
          <div style="background: #ffebee; padding: 15px; border-radius: 8px; margin-top: 10px; border-left: 4px solid #f44336;">
            <p style="margin: 0; color: #c62828;">
              <i class="fas fa-exclamation-triangle"></i> <strong>Cảnh báo:</strong> Hành động này sẽ xóa mềm khóa học. Bạn có chắc chắn muốn tiếp tục?
            </p>
          </div>
        </div>
      `,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#f44336',
      width: '600px',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.khoaHocService.softDelete(this.id).subscribe({
        next: () => {
          Swal.fire('Thành công', 'Đã xóa khóa học thành công', 'success').then(() => {
            this.router.navigate(['/admin/khoa-hoc']).then();
          });
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể xóa khóa học';
          Swal.fire('Lỗi', msg, 'error');
        },
      });
    });
  }
}
