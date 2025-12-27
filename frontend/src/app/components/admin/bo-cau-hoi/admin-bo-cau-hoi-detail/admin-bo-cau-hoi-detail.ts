import {CommonModule, NgClass} from '@angular/common';
import {Component, OnInit} from '@angular/core';
import Swal from 'sweetalert2';
import {BoCauHoiResponse} from '../../../../responses/bocauhoi/bocauhoi-response';
import {CauHoiResponse} from '../../../../responses/cauhoi/cauhoi-response';
import {PageResponse} from '../../../../responses/page-response';
import {ResponseObject} from '../../../../responses/response-object';
import {Base} from '../../../base/base';
import {NgbDropdownModule} from '@ng-bootstrap/ng-bootstrap';

@Component({
  selector: 'app-admin-bo-cau-hoi-detail',
  imports: [NgClass, CommonModule, NgbDropdownModule],
  templateUrl: './admin-bo-cau-hoi-detail.html',
  styleUrl: './admin-bo-cau-hoi-detail.scss',
  standalone: true,
})
export class AdminBoCauHoiDetail extends Base implements OnInit {
  bo_cau_hoi?: BoCauHoiResponse;
  cau_hoi_list: CauHoiResponse[] = [];

  loading = false;
  id!: number;

  // Preview modal
  previewQuestion?: CauHoiResponse;
  showPreviewModal = false;

  ngOnInit(): void {
    this.route.paramMap.subscribe((params) => {
      this.id = Number(params.get('id'));
      if (this.id) {
        this.loadBoCauHoi();
        this.loadCauHoi();
      }
    });
  }

  loadBoCauHoi(): void {
    this.bocauHoiService.getById(this.id).subscribe({
      next: (res: ResponseObject<BoCauHoiResponse>) => {
        console.log(res.data);
        this.bo_cau_hoi = res.data!;
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải thông tin bộ câu hỏi', 'error').then((r) => {
        });
      },
    });
  }

  loadCauHoi(): void {
    this.cauHoiService.getByBoCauHoi(this.id).subscribe({
      next: (res: ResponseObject<PageResponse<CauHoiResponse>>) => {
        this.cau_hoi_list = res.data?.items ?? [];
        this.loading = false;
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách câu hỏi', 'error').then((r) => {
        });
      },
    });
  }

  /**
   * Tính giá gợi ý dựa trên số câu hỏi (giống logic backend)
   * < 20 câu = 50G
   * 20-49 câu = 100G
   * >= 50 câu = 150G
   */
  suggestGiaMoKhoa(soCau: number): number {
    if (soCau < 20) return 50;
    if (soCau < 50) return 100;
    return 150;
  }

  approveBo(): void {
    if (!this.bo_cau_hoi) return;

    const soCau = this.cau_hoi_list.length;
    const muonTraPhi = this.bo_cau_hoi.muon_tao_tra_phi;
    const giaGoiY = muonTraPhi ? this.suggestGiaMoKhoa(soCau) : 0;

    let htmlContent = `
      <div style="text-align: left; padding: 10px 0;">
        <p><strong>Bộ câu hỏi:</strong> ${this.bo_cau_hoi.tieu_de}</p>
        <p><strong>Số câu hỏi:</strong> ${soCau} câu</p>
        <p><strong>Người tạo muốn:</strong> ${
      muonTraPhi
        ? '<span style="color: #ff9800;">💰 Trả phí</span>'
        : '<span style="color: #4caf50;">🎁 Miễn phí</span>'
    }</p>
    `;

    if (muonTraPhi) {
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
      title: 'Duyệt bộ câu hỏi này?',
      html: htmlContent,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Duyệt',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#4caf50',
      width: '600px',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.bocauHoiService.approveBoCauHoi(this.id).subscribe({
        next: (res: any) => {
          Swal.fire('Thành công', 'Đã duyệt bộ câu hỏi', 'success');
          this.loadBoCauHoi();
        },
        error: (err) => {
          console.log('🔥 Check lỗi:', err); // Log để kiểm tra

          // 1. Mặc định
          let hienThiLoi = 'Không thể duyệt bộ câu hỏi';

          // 2. Trường hợp Backend trả về JSON chuẩn (nếu bạn fix được backend về 400)
          if (err.error && typeof err.error === 'object' && err.error.message) {
            hienThiLoi = err.error.message;
          }

          // 3. Trường hợp Backend trả về String (như trong ảnh bạn gửi: lỗi 401 kèm text dài)
          else if (typeof err.error === 'string') {
            // Chuỗi lỗi: "Unauthorized... java.lang.IllegalArgumentException: <NỘI DUNG CẦN LẤY>"
            if (err.error.includes('IllegalArgumentException:')) {
              // Cắt chuỗi để lấy phần nội dung tiếng Việt phía sau
              const parts = err.error.split('IllegalArgumentException:');
              if (parts.length > 1) {
                hienThiLoi = parts[1].trim(); // Lấy phần sau và xóa khoảng trắng thừa
              }
            } else {
              hienThiLoi = err.error; // Nếu không tìm thấy format kia thì hiện nguyên văn
            }
          }

          // 4. Hiển thị thông báo
          Swal.fire('Không thể duyệt', hienThiLoi, 'error');
        },
      });
    });
  }

  rejectBo(): void {
    if (!this.bo_cau_hoi) return;

    Swal.fire({
      title: 'Từ chối bộ câu hỏi',
      text: this.bo_cau_hoi.tieu_de,
      input: 'text',
      inputPlaceholder: 'Nhập lý do từ chối...',
      inputValidator: (v) => (!v || !v.trim() ? 'Vui lòng nhập lý do' : null),
      showCancelButton: true,
      confirmButtonText: 'Từ chối',
      cancelButtonText: 'Hủy',
      icon: 'warning',
    }).then((result) => {
      if (!result.isConfirmed || !result.value) return;

      const reason = result.value.trim();

      this.bocauHoiService.rejectBoCauHoi(this.id, reason).subscribe({
        next: () => {
          Swal.fire('Đã từ chối', 'Đã cập nhật trạng thái bộ câu hỏi', 'success').then((r) => {
          });
          this.loadBoCauHoi(); // để thấy trạng thái TU_CHOI + lý do
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể từ chối bộ câu hỏi', 'error').then((r) => {
          });
        },
      });
    });
  }

  deleteBo(): void {
    if (!this.bo_cau_hoi) return;
    Swal.fire({
      title: 'Xoá bộ câu hỏi?',
      text: this.bo_cau_hoi.tieu_de,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xoá',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.bocauHoiService.delete(this.id).subscribe({
        next: () => {
          Swal.fire('Đã xoá', 'Bộ câu hỏi đã được xoá', 'success').then((r) => {
          });
          this.router.navigate(['/admin/bo-cau-hoi']).then((r) => {
          });
        },
        error: (err) => {
          const msg = err?.error?.message;

          if (msg?.includes('trận đấu')) {
            Swal.fire(
              'Không thể xoá',
              'Bộ câu hỏi này đã được sử dụng trong các trận đấu, không thể xoá.',
              'error'
            ).then((r) => {
            });
          } else {
            Swal.fire('Lỗi', msg || 'Không thể xoá bộ câu hỏi', 'error').then((r) => {
            });
          }
        },
      });
    });
  }

  markOfficialBo(): void {
    if (!this.bo_cau_hoi) return;

    const newState = !this.bo_cau_hoi.is_official; // toggle
    const actionText = newState
      ? 'Đánh dấu là bộ câu hỏi thi đấu (official)'
      : 'Bỏ đánh dấu official';

    Swal.fire({
      title: actionText + '?',
      text: this.bo_cau_hoi.tieu_de,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Xác nhận',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.bocauHoiService.markOfficial(this.id).subscribe({
        next: () => {
          Swal.fire('Thành công', 'Đã cập nhật cờ official', 'success').then((r) => {
          });
          this.loadBoCauHoi(); // reload để cập nhật is_official mới
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể cập nhật cờ official';
          Swal.fire('Lỗi', msg, 'error').then((r) => {
          });
        },
      });
    });
  }

  disMarkOfficialBo(): void {
    if (!this.bo_cau_hoi) return;

    const newState = !this.bo_cau_hoi.is_official; // toggle
    const actionText = newState
      ? 'Bỏ đánh dấu official'
      : 'Đánh dấu là bộ câu hỏi thi đấu (official)';

    Swal.fire({
      title: actionText + '?',
      text: this.bo_cau_hoi.tieu_de,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Xác nhận',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.bocauHoiService.disMarkOfficial(this.id).subscribe({
        next: () => {
          Swal.fire('Thành công', 'Đã cập nhật hủy cờ official', 'success').then((r) => {
          });
          this.loadBoCauHoi(); // reload để cập nhật is_official mới
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể cập nhật cờ official';
          Swal.fire('Lỗi', msg, 'error').then((r) => {
          });
        },
      });
    });
  }

  get co_quyen_sua(): boolean {
    return !!this.bo_cau_hoi?.co_quyen_sua;
  }

  goBack(): void {
    this.router.navigate(['/admin/bo-cau-hoi']).then((r) => {
    });
  }

  goToEditQuestion(cauHoiId: number): void {
    // sang màn chỉnh sửa câu hỏi admin
    this.router.navigate(['/admin/cau-hoi', cauHoiId, 'chinh-sua']).then(() => {
    });
  }

  onDeleteQuestion(id: number): void {
    Swal.fire({
      title: 'Xác nhận xóa?',
      text: 'Bạn có chắc muốn xóa câu hỏi này không?',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.cauHoiService.delete(id).subscribe({
        next: () => {
          Swal.fire('Thành công', 'Xóa câu hỏi thành công', 'success').then(() => {
          });
          // load lại danh sách câu hỏi trong bộ
          this.loadCauHoi();
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể xóa câu hỏi', 'error').then(() => {
          });
        },
      });
    });
  }

  goToEditBo(): void {
    this.router.navigate(['/admin/bo-cau-hoi/sua-bo-cau-hoi', this.id]).then((r) => {
    });
  }

  goToAddQuestion(): void {
    this.router.navigate(['/admin/bo-cau-hoi', this.id, 'cau-hoi', 'tao-moi']).then((r) => {
    });
  }

  formatDate(dateString?: string): string {
    if (!dateString) return '-';
    try {
      const date = new Date(dateString);
      return date.toLocaleDateString('vi-VN', {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
        hour: '2-digit',
        minute: '2-digit',
      });
    } catch {
      return dateString;
    }
  }

  // Preview câu hỏi
  previewQuestionDetail(question: CauHoiResponse): void {
    this.previewQuestion = question;
    this.showPreviewModal = true;
  }

  closePreview(): void {
    this.showPreviewModal = false;
    this.previewQuestion = undefined;
  }

  // Duplicate bộ câu hỏi
  duplicateBo(): void {
    if (!this.bo_cau_hoi) return;

    Swal.fire({
      title: 'Duplicate bộ câu hỏi',
      html: `
        <div style="text-align: left; padding: 10px 0;">
          <p><strong>Bộ câu hỏi gốc:</strong> ${this.bo_cau_hoi.tieu_de}</p>
          <p><strong>Số câu hỏi:</strong> ${this.cau_hoi_list.length} câu</p>
          <p><strong>Mục đích duplicate:</strong></p>
          <select id="duplicate-purpose" style="width: 100%; padding: 8px; margin: 10px 0; border: 1px solid #ddd; border-radius: 4px;">
            <option value="COURSE">Dùng cho khóa học (COURSE_ONLY)</option>
            <option value="RANKED">Dùng cho thi đấu ranked (RANKED_ONLY)</option>
          </select>
          <div style="background: #e3f2fd; padding: 15px; border-radius: 8px; margin-top: 10px; border-left: 4px solid #2196f3;">
            <p style="margin: 0; color: #1565c0;">
              <i class="fas fa-info-circle"></i>
              <strong>Khóa học:</strong> Bộ câu hỏi sẽ được set loại COURSE_ONLY, có thể thêm vào khóa học.<br>
              <strong>Ranked:</strong> Bộ câu hỏi sẽ được set loại RANKED_ONLY, isOfficial=true, và người tạo sẽ nhận 200 gold + 100 exp + achievement.
            </p>
          </div>
        </div>
      `,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Duplicate',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#2196f3',
      width: '700px',
      didOpen: () => {
        // Focus vào select
        const select = document.getElementById('duplicate-purpose') as HTMLSelectElement;
        if (select) select.focus();
      },
      preConfirm: () => {
        const select = document.getElementById('duplicate-purpose') as HTMLSelectElement;
        return select?.value || null;
      },
    }).then((result) => {
      if (!result.isConfirmed || !result.value) return;

      const purpose = result.value; // "COURSE" hoặc "RANKED"
      const loaiSuDung = purpose === 'COURSE' ? 'COURSE_ONLY' : 'RANKED_ONLY';

      this.bocauHoiService.duplicate(this.id, loaiSuDung, purpose).subscribe({
        next: (res: ResponseObject<BoCauHoiResponse>) => {
          Swal.fire('Thành công', 'Đã duplicate bộ câu hỏi thành công', 'success').then(() => {
            // Navigate to the duplicated question set
            if (res.data?.id) {
              this.router.navigate(['/admin/bo-cau-hoi', res.data.id]).then(() => {
                this.loadBoCauHoi();
                this.loadCauHoi();
              });
            }
          });
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể duplicate bộ câu hỏi';
          Swal.fire('Lỗi', msg, 'error');
        },
      });
    });
  }

  getUsageClass(type: string | undefined): string {
    if (type === 'RANKED_ONLY') return 'purple';
    if (type === 'CASUAL_ONLY') return 'blue';
    if (type === 'PRACTICE_ONLY') return 'green';
    return '';
  }

  getUsageIcon(type: string | undefined): string {
    if (type === 'RANKED_ONLY') return 'fas fa-trophy';
    if (type === 'CASUAL_ONLY') return 'fas fa-gamepad';
    if (type === 'PRACTICE_ONLY') return 'fas fa-book';
    return '';
  }

  mapTrangThai(st: string): string {
    switch (st) {
      case 'CHO_DUYET':
        return 'Chờ duyệt';
      case 'DA_DUYET':
        return 'Đã duyệt';
      case 'TU_CHOI':
        return 'Đã từ chối';
      default:
        return st;
    }
  }

  mapDoKho(d: string): string {
    switch (d) {
      case 'DE':
        return 'Dễ';
      case 'TRUNG_BINH':
        return 'Trung bình';
      case 'KHO':
        return 'Khó';
      default:
        return d;
    }
  }
}
