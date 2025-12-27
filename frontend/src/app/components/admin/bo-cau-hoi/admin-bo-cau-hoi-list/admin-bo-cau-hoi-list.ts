import {CommonModule} from '@angular/common';
import {Component, OnInit} from '@angular/core';
import {FormsModule} from '@angular/forms';
import Swal from 'sweetalert2';
import {ChuDe} from '../../../../models/chude';
import {BoCauHoiResponse} from '../../../../responses/bocauhoi/bocauhoi-response';
import {PageResponse} from '../../../../responses/page-response';
import {ResponseObject} from '../../../../responses/response-object';
import {Base} from '../../../base/base';
import {NgbDropdownModule} from '@ng-bootstrap/ng-bootstrap';

@Component({
  selector: 'app-admin-bo-cau-hoi-list',
  imports: [CommonModule, FormsModule, NgbDropdownModule],
  templateUrl: './admin-bo-cau-hoi-list.html',
  styleUrl: './admin-bo-cau-hoi-list.scss',
  standalone: true,
})
export class AdminBoCauHoiList extends Base implements OnInit {
  loading = false;

  // filter
  keyword = '';
  chuDeId: number = 0;
  trangThai: string = 'CHO_DUYET'; // mặc định chỉ xem CHỜ DUYỆT
  loaiSuDung: string = '';
  muonTaoTraPhi?: boolean;
  nguoiTaoId: number = 0;
  sortOrder = 'NEWEST';

  // paging
  page = 0;
  limit = 10;
  totalPages = 0;

  items: BoCauHoiResponse[] = [];
  chuDes: ChuDe[] = [];
  selectedItems: Set<number> = new Set();
  statistics: any = null;

  showAdvancedFilter = false; // Biến toggle filter

  readonly trangThaiOptions = [
    {value: 'CHO_DUYET', label: 'Chờ duyệt'},
    {value: 'DA_DUYET', label: 'Đã duyệt'},
    {value: 'TU_CHOI', label: 'Từ chối'},
    {value: '', label: 'Tất cả'},
  ];

  readonly loaiSuDungOptions = [
    {value: '', label: 'Tất cả'},
    {value: 'RANKED_ONLY', label: 'Ranked Only'},
    {value: 'CASUAL_ONLY', label: 'Casual Only'},
    {value: 'PRACTICE_ONLY', label: 'Practice Only'},
  ];

  readonly loaiOptions = [
    {value: undefined, label: 'Tất cả'},
    {value: true, label: 'Trả phí'},
    {value: false, label: 'Miễn phí'},
  ];

  readonly sortOptions = [
    {value: 'NEWEST', label: 'Mới nhất'},
    {value: 'OLDEST', label: 'Cũ nhất'},
  ];

  ngOnInit(): void {
    this.loadChuDe();
    this.loadData();
    this.loadStatistics();
  }

  loadChuDe(): void {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<ChuDe[]>) => {
        this.chuDes = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error').then((r) => {
        });
      },
    });
  }

  loadData(): void {
    this.loading = true;
    this.bocauHoiService
      .getAll(
        this.keyword,
        this.chuDeId,
        '', // che_do_hien_thi: để backend tự xử lý
        this.trangThai,
        this.loaiSuDung,
        this.muonTaoTraPhi,
        this.nguoiTaoId,
        this.sortOrder,
        this.page,
        this.limit
      )
      .subscribe({
        next: (res: ResponseObject<PageResponse<BoCauHoiResponse>>) => {
          const data = res.data!;
          this.items = data.items ?? [];
          this.totalPages = data.totalPages;
          this.loading = false;
          this.selectedItems.clear(); // Clear selection khi load lại
        },
        error: () => {
          this.loading = false;
          Swal.fire('Lỗi', 'Không thể tải danh sách bộ câu hỏi', 'error').then((r) => {
          });
        },
      });
  }

  loadStatistics(): void {
    this.bocauHoiService.getStatistics().subscribe({
      next: (res: ResponseObject<any>) => {
        this.statistics = res.data;
      },
      error: () => {
        // Silent fail for statistics
      },
    });
  }

  /** 🔄 Đổi trang */
  changePage(newPage: number): void {
    if (newPage < 0 || newPage >= this.totalPages) return;
    this.page = newPage;
    this.loadData();
  }

  /**
   * Tính giá gợi ý dựa trên số câu hỏi (giống logic backend)
   */
  suggestGiaMoKhoa(soCau: number): number {
    if (soCau < 20) return 50;
    if (soCau < 50) return 100;
    return 150;
  }

  /** ✅ Duyệt bộ câu hỏi */
  approve(quiz: BoCauHoiResponse): void {
    const muonTraPhi = quiz.muon_tao_tra_phi;
    const soCau = quiz.so_cau_hoi || 0;
    const giaGoiY = muonTraPhi && soCau > 0 ? this.suggestGiaMoKhoa(soCau) : 0;

    let htmlContent = `
      <div style="text-align: left; padding: 10px 0;">
        <p><strong>Bộ câu hỏi:</strong> ${quiz.tieu_de}</p>
        <p><strong>Số câu hỏi:</strong> ${soCau} câu</p>
        <p><strong>Người tạo muốn:</strong> ${
      muonTraPhi
        ? '<span style="color: #ff9800;">💰 Trả phí</span>'
        : '<span style="color: #4caf50;">🎁 Miễn phí</span>'
    }</p>
    `;

    if (muonTraPhi && soCau > 0) {
      htmlContent += `
        <div style="background: #fff8e1; padding: 15px; border-radius: 8px; margin-top: 10px; border-left: 4px solid #ff9800;">
          <p style="margin: 0 0 5px 0;"><strong>💰 Giá mở khóa sẽ được set:</strong></p>
          <p style="margin: 0; font-size: 1.2em; color: #ff6f00;"><strong>${giaGoiY} vàng</strong></p>
          <p style="margin: 5px 0 0 0; font-size: 0.9em; color: #666;">
            (Dựa trên số câu hỏi: ${
        soCau < 20 ? '< 20 câu = 50G' : soCau < 50 ? '20-49 câu = 100G' : '≥ 50 câu = 150G'
      })
          </p>
          <p style="margin: 10px 0 0 0; font-size: 0.85em; color: #666;">
            <i class="fas fa-info-circle"></i> Người tạo sẽ nhận <strong>70%</strong> số vàng mỗi khi có người chơi mở khóa.
          </p>
        </div>
      `;
    } else if (muonTraPhi && soCau === 0) {
      htmlContent += `
        <div style="background: #ffebee; padding: 15px; border-radius: 8px; margin-top: 10px; border-left: 4px solid #f44336;">
          <p style="margin: 0; color: #c62828;">
            <i class="fas fa-exclamation-triangle"></i> <strong>Cảnh báo:</strong> Bộ câu hỏi chưa có câu hỏi nào. Vui lòng thêm câu hỏi trước khi duyệt.
          </p>
        </div>
      `;
    } else {
      htmlContent += `
        <div style="background: #e8f5e9; padding: 15px; border-radius: 8px; margin-top: 10px; border-left: 4px solid #4caf50;">
          <p style="margin: 0; color: #2e7d32;">
            <i class="fas fa-gift"></i> Bộ câu hỏi sẽ được phát hành <strong>miễn phí</strong>.
          </p>
        </div>
      `;
    }

    htmlContent += `</div>`;

    Swal.fire({
      title: 'Xác nhận duyệt bộ câu hỏi?',
      html: htmlContent,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Duyệt',
      cancelButtonText: 'Huỷ',
      confirmButtonColor: '#4caf50',
      width: '600px',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.bocauHoiService.approveBoCauHoi(quiz.id).subscribe({
        next: (res: ResponseObject<BoCauHoiResponse>) => {
          Swal.fire('Thành công', res.message || 'Đã duyệt bộ câu hỏi', 'success').then((r) => {
          });
          this.loadData();
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể duyệt bộ câu hỏi';
          Swal.fire('Lỗi', msg, 'error').then((r) => {
          });
        },
      });
    });
  }

  /** ❌ Từ chối bộ câu hỏi */
  reject(quiz: BoCauHoiResponse): void {
    Swal.fire({
      title: 'Lý do từ chối',
      input: 'text',
      inputPlaceholder: 'Nhập lý do từ chối...',
      inputValidator: (value) => {
        if (!value || !value.trim()) {
          return 'Vui lòng nhập lý do từ chối';
        }
        return null;
      },
      showCancelButton: true,
      confirmButtonText: 'Từ chối',
      cancelButtonText: 'Huỷ',
      icon: 'warning',
    }).then((result) => {
      if (!result.isConfirmed || !result.value) return;

      const reason = result.value.trim();
      this.bocauHoiService.rejectBoCauHoi(quiz.id, reason).subscribe({
        next: (res: ResponseObject<BoCauHoiResponse>) => {
          Swal.fire('Đã từ chối', res.message || 'Đã từ chối bộ câu hỏi', 'success').then(
            (r) => {
            }
          );
          this.loadData();
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể từ chối bộ câu hỏi';
          Swal.fire('Lỗi', msg, 'error').then((r) => {
          });
        },
      });
    });
  }

  navigateDetail(id: number) {
    // Angular Router tự ghép mảng thành url /admin/bo-cau-hoi/123
    this.router.navigate(['/admin/bo-cau-hoi', id]).then((r) => {
    });
  }

  goToCreateAdminBo(): void {
    console.log('Navigate to create new Bo Cau Hoi');
    // this.router.navigate('/admin/bo-cau-hoi/tao-moi').then(r => {
    // });
    this.router.navigate(['/admin/bo-cau-hoi/tao-moi']).then((r) => {
    });
  }

  // Bulk actions
  toggleSelect(id: number): void {
    if (this.selectedItems.has(id)) {
      this.selectedItems.delete(id);
    } else {
      this.selectedItems.add(id);
    }
  }

  toggleSelectAll(): void {
    if (this.selectedItems.size === this.items.length) {
      this.selectedItems.clear();
    } else {
      this.items.forEach((item) => this.selectedItems.add(item.id));
    }
  }

  get hasSelection(): boolean {
    return this.selectedItems.size > 0;
  }

  bulkApprove(): void {
    if (!this.hasSelection) return;

    const ids = Array.from(this.selectedItems);
    Swal.fire({
      title: `Duyệt ${ids.length} bộ câu hỏi?`,
      text: 'Bạn có chắc chắn muốn duyệt tất cả các bộ câu hỏi đã chọn?',
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Duyệt',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.bocauHoiService.bulkApprove(ids).subscribe({
        next: (res: ResponseObject<any>) => {
          const data = res.data;
          Swal.fire({
            title: 'Hoàn thành',
            html: `
              <p>Đã duyệt thành công: <strong>${data.successCount}</strong> bộ câu hỏi</p>
              ${
              data.failCount > 0
                ? `<p>Thất bại: <strong>${data.failCount}</strong> bộ câu hỏi</p>`
                : ''
            }
            `,
            icon: data.failCount > 0 ? 'warning' : 'success',
          });
          this.selectedItems.clear();
          this.loadData();
          this.loadStatistics();
        },
        error: (err) => {
          Swal.fire('Lỗi', err.error?.message || 'Không thể duyệt hàng loạt', 'error');
        },
      });
    });
  }

  bulkReject(): void {
    if (!this.hasSelection) return;

    const ids = Array.from(this.selectedItems);
    Swal.fire({
      title: 'Lý do từ chối',
      input: 'text',
      inputPlaceholder: 'Nhập lý do từ chối...',
      inputValidator: (value) => {
        if (!value || !value.trim()) {
          return 'Vui lòng nhập lý do từ chối';
        }
        return null;
      },
      showCancelButton: true,
      confirmButtonText: 'Từ chối',
      cancelButtonText: 'Hủy',
      icon: 'warning',
    }).then((result) => {
      if (!result.isConfirmed || !result.value) return;

      const lyDo = result.value.trim();
      this.bocauHoiService.bulkReject(ids, lyDo).subscribe({
        next: (res: ResponseObject<any>) => {
          const data = res.data;
          Swal.fire({
            title: 'Hoàn thành',
            html: `
              <p>Đã từ chối thành công: <strong>${data.successCount}</strong> bộ câu hỏi</p>
              ${
              data.failCount > 0
                ? `<p>Thất bại: <strong>${data.failCount}</strong> bộ câu hỏi</p>`
                : ''
            }
            `,
            icon: data.failCount > 0 ? 'warning' : 'success',
          });
          this.selectedItems.clear();
          this.loadData();
          this.loadStatistics();
        },
        error: (err) => {
          Swal.fire('Lỗi', err.error?.message || 'Không thể từ chối hàng loạt', 'error');
        },
      });
    });
  }

  resetFilters() {
    this.keyword = '';
    this.chuDeId = 0;
    this.trangThai = 'CHO_DUYET';
    this.loaiSuDung = '';
    this.muonTaoTraPhi = undefined;
    this.sortOrder = 'NEWEST';
    this.showAdvancedFilter = false;
    this.page = 0;
    this.loadData();
  }

  getStatusLabel(status: string): string {
    switch (status) {
      case 'CHO_DUYET':
        return 'Chờ duyệt';
      case 'DA_DUYET':
        return 'Đã duyệt';
      case 'TU_CHOI':
        return 'Từ chối';
      default:
        return status;
    }
  }

  // Helper check all items in current page
  get isAllSelected(): boolean {
    return this.items.length > 0 && this.items.every(i => this.selectedItems.has(i.id));
  }

  navigateEdit(id: number) {
    this.router.navigate(['/admin/bo-cau-hoi/sua-bo-cau-hoi', id]).then((r) => {
    });
  }
}
