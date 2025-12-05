import {Component, OnInit} from '@angular/core';
import {Base} from '../../../base/base';
import {BoCauHoiResponse} from '../../../../responses/bocauhoi/bocauhoi-response';
import {CauHoiResponse} from '../../../../responses/cauhoi/cauhoi-response';
import {ResponseObject} from '../../../../responses/response-object';
import Swal from 'sweetalert2';
import {PageResponse} from '../../../../responses/page-response';
import {NgClass} from '@angular/common';

@Component({
  selector: 'app-admin-bo-cau-hoi-detail',
  imports: [
    NgClass
  ],
  templateUrl: './admin-bo-cau-hoi-detail.html',
  styleUrl: './admin-bo-cau-hoi-detail.scss',
  standalone: true
})
export class AdminBoCauHoiDetail extends Base implements OnInit {
  bo_cau_hoi?: BoCauHoiResponse;
  cau_hoi_list: CauHoiResponse[] = [];

  loading = false;
  id!: number;

  ngOnInit(): void {
    this.route.paramMap.subscribe(params => {
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
        Swal.fire('Lỗi', 'Không thể tải thông tin bộ câu hỏi', 'error').then(r => {
        });
      }
    });
  }

  loadCauHoi(): void {
    this.cauHoiService.getByBoCauHoi(this.id).subscribe({
      next: (res: ResponseObject<PageResponse<CauHoiResponse>>) => {
        this.cau_hoi_list = res.data?.items ?? [];
        this.loading = false;
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách câu hỏi', 'error').then(r => {
        });
      }
    });
  }

  approveBo(): void {
    if (!this.bo_cau_hoi) return;

    Swal.fire({
      title: 'Duyệt bộ câu hỏi này?',
      text: this.bo_cau_hoi.tieu_de,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Duyệt',
      cancelButtonText: 'Hủy'
    }).then(result => {
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
        }
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
      icon: 'warning'
    }).then(result => {
      if (!result.isConfirmed || !result.value) return;

      const reason = result.value.trim();

      this.bocauHoiService.rejectBoCauHoi(this.id, reason).subscribe({
        next: () => {
          Swal.fire('Đã từ chối', 'Đã cập nhật trạng thái bộ câu hỏi', 'success').then(r => {
          });
          this.loadBoCauHoi(); // để thấy trạng thái TU_CHOI + lý do
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể từ chối bộ câu hỏi', 'error').then(r => {
          });
        }
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
      cancelButtonText: 'Hủy'
    }).then(result => {
      if (!result.isConfirmed) return;

      this.bocauHoiService.delete(this.id).subscribe({
        next: () => {
          Swal.fire('Đã xoá', 'Bộ câu hỏi đã được xoá', 'success').then(r => {
          });
          this.router.navigate(['/admin/bo-cau-hoi']).then(r => {
          });
        },
        error: (err) => {
          const msg = err?.error?.message;

          if (msg?.includes('trận đấu')) {
            Swal.fire(
              'Không thể xoá',
              'Bộ câu hỏi này đã được sử dụng trong các trận đấu, không thể xoá.',
              'error'
            ).then(r => {
            });
          } else {
            Swal.fire('Lỗi', msg || 'Không thể xoá bộ câu hỏi', 'error').then(r => {
            });
          }
        }
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
      cancelButtonText: 'Hủy'
    }).then(result => {
      if (!result.isConfirmed) return;

      this.bocauHoiService.markOfficial(this.id).subscribe({
        next: () => {
          Swal.fire('Thành công', 'Đã cập nhật cờ official', 'success').then(r => {
          });
          this.loadBoCauHoi(); // reload để cập nhật is_official mới
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể cập nhật cờ official';
          Swal.fire('Lỗi', msg, 'error').then(r => {
          });
        }
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
      cancelButtonText: 'Hủy'
    }).then(result => {
      if (!result.isConfirmed) return;

      this.bocauHoiService.disMarkOfficial(this.id).subscribe({
        next: () => {
          Swal.fire('Thành công', 'Đã cập nhật hủy cờ official', 'success').then(r => {
          });
          this.loadBoCauHoi(); // reload để cập nhật is_official mới
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể cập nhật cờ official';
          Swal.fire('Lỗi', msg, 'error').then(r => {
          });
        }
      });
    });
  }


  get co_quyen_sua(): boolean {
    return !!this.bo_cau_hoi?.co_quyen_sua;
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


  goBack(): void {
    this.router.navigate(['/admin/bo-cau-hoi']).then(r => {
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
      cancelButtonText: 'Hủy'
    }).then(result => {
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
        }
      });
    });
  }

  goToEditBo(): void {
    this.router.navigate(['/admin/bo-cau-hoi/sua-bo-cau-hoi', this.id]).then(r => {
    });
  }

  goToAddQuestion(): void {
    this.router.navigate(['/admin/bo-cau-hoi', this.id, 'cau-hoi', 'tao-moi']).then(r => {
    });
  }

}
