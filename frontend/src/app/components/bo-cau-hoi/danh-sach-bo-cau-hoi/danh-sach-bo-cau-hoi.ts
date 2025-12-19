import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import Swal from 'sweetalert2';
import { ChuDe } from '../../../models/chude';
import { BoCauHoiResponse } from '../../../responses/bocauhoi/bocauhoi-response';
import { PageResponse } from '../../../responses/page-response';
import { ResponseObject } from '../../../responses/response-object';
import { Base } from '../../base/base';

@Component({
  selector: 'app-bo-cau-hoi-danh-sach-bo-cau-hoi',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './danh-sach-bo-cau-hoi.html',
  styleUrl: './danh-sach-bo-cau-hoi.scss',
})
export class BoCauHoiList extends Base implements OnInit {
  loading = false;
  keyword = '';
  cheDoHienThi = '';
  trangThai = '';
  chuDeId = 0;
  page = 0;
  limit = 3;
  sortOrder = 'NEWEST';
  totalPages = 0;
  currentUserId: number = 0;
  items: BoCauHoiResponse[] = [];
  unlocking_id: number | null = null;

  chuDes: ChuDe[] = [];
  readonly trangThaiOptions = [
    { value: '', label: 'Tất cả' },
    { value: 'DA_DUYET', label: 'Đã duyệt' },
    { value: 'CHO_DUYET', label: 'Chờ duyệt' },
    { value: 'TU_CHOI', label: 'Từ chối' },
  ];

  ngOnInit() {
    this.currentUserId = this.tokenService.getUserId();
    this.loadData();
    this.loadChuDe();
  }

  loadData() {
    this.loading = true;
    this.bocauHoiService
      .getAll(
        this.keyword,
        this.chuDeId,
        this.cheDoHienThi,
        this.trangThai,
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
        },
        error: () => {
          this.loading = false;
          Swal.fire('Lỗi', 'Không thể tải danh sách bộ câu hỏi', 'error').then((r) => {});
        },
      });
  }

  loadChuDe() {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<any>) => {
        this.chuDes = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error').then((r) => {});
      },
    });
  }

  /** 🧠 Kiểm tra quyền sửa */
  canEdit(quiz: BoCauHoiResponse): boolean {
    return quiz.nguoi_tao_id === this.currentUserId;
  }

  /** 🧠 Kiểm tra quyền xóa */
  canDelete(quiz: BoCauHoiResponse): boolean {
    return quiz.nguoi_tao_id === this.currentUserId;
  }

  /** ⚠️ Hiện cảnh báo khi không đủ quyền */
  showAccessDeniedAlert(action: string) {
    Swal.fire({
      icon: 'warning',
      title: `Không thể ${action}`,
      text: `Bạn không thể ${action} bộ câu hỏi của người khác hoặc của admin!`,
      confirmButtonColor: '#3085d6',
      confirmButtonText: 'Đã hiểu',
    }).then((r) => {});
  }

  onSearch() {
    this.page = 0;
    this.loadData();
  }

  applyFilter() {
    this.page = 0;
    this.loadData();
  }

  highlightKeyword(text: string): string {
    if (!this.keyword) return text;
    const regex = new RegExp(`(${this.keyword})`, 'gi');
    return text.replace(regex, '<mark>$1</mark>');
  }

  clearFilter() {
    this.keyword = '';
    this.cheDoHienThi = '';
    this.trangThai = '';
    this.chuDeId = 0;
    this.loadData();
  }

  goToCreateQuiz() {
    // sau này sẽ điều hướng đến trang tạo bộ câu hỏi
    this.router.navigateByUrl('/bo-cau-hoi/tao-moi-bo-cau-hoi').then((r) => {});
  }

  getVisiblePages(): number[] {
    const visible: number[] = [];
    const maxVisible = 7; // số nút trang hiển thị tối đa
    const total = this.totalPages;

    if (total <= maxVisible) {
      return Array.from({ length: total }, (_, i) => i);
    }

    const start = Math.max(0, this.page - 3);
    const end = Math.min(total - 1, this.page + 3);

    // luôn hiển thị trang đầu
    if (start > 0) visible.push(0);

    // nếu cách xa đầu -> thêm dấu ...
    if (start > 1) visible.push(-1);

    for (let i = start; i <= end; i++) visible.push(i);

    // nếu cách xa cuối -> thêm dấu ...
    if (end < total - 2) visible.push(-2);

    // luôn hiển thị trang cuối
    if (end < total - 1) visible.push(total - 1);

    return visible;
  }

  changePage(p: number) {
    if (p < 0 || p >= this.totalPages) return;
    this.page = p;
    this.loadData();
  }

  navigateDetail(id: number) {
    this.router.navigate(['/bo-cau-hoi/chi-tiet-bo-cau-hoi', id]).then((r) => {});
  }

  confirmDelete(id: number) {
    Swal.fire({
      title: 'Xác nhận xóa?',
      text: 'Bộ câu hỏi này và toàn bộ câu hỏi trong đó sẽ bị xóa vĩnh viễn!',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#e11d48',
    }).then((result) => {
      if (result.isConfirmed) {
        this.deleteBoCauHoi(id);
      }
    });
  }

  deleteBoCauHoi(id: number) {
    this.bocauHoiService.delete(id).subscribe({
      next: (res) => {
        Swal.fire('Thành công', res.message || 'Xóa thành công', 'success').then((r) => {});
        this.loadData(); // reload lại danh sách
      },
      error: (err) => {
        Swal.fire('Lỗi', err.error?.message || 'Không thể xóa bộ câu hỏi', 'error').then((r) => {});
      },
    });
  }

  markOfficial(quiz: BoCauHoiResponse) {
    Swal.fire({
      title: 'Xác nhận',
      text: 'Gắn Official cho bộ câu hỏi này? (Cần ít nhất 5 câu hỏi và đã được duyệt)',
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Đồng ý',
      cancelButtonText: 'Huỷ',
    }).then((result) => {
      if (!result.isConfirmed) {
        return;
      }

      this.bocauHoiService.markOfficial(quiz.id).subscribe({
        next: (res: ResponseObject<BoCauHoiResponse>) => {
          Swal.fire('Thành công', res.message || 'Đã gắn Official', 'success').then((r) => {});
          this.loadData(); // hàm bạn đang dùng để reload danh sách
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể gắn Official';
          Swal.fire('Lỗi', msg, 'error').then((r) => {});
        },
      });
    });
  }

  navigateEdit(id: number) {
    this.router.navigate(['/bo-cau-hoi/sua-bo-cau-hoi', id]).then((r) => {});
  }

  /** Click nút Luyện / Mở khoá */
  handlePracticeClick(quiz: BoCauHoiResponse) {
    // Nếu không cần mở khoá hoặc đã mở rồi -> đi luyện luôn
    if (!quiz.can_mo_khoa || quiz.da_mo_khoa) {
      this.navigatePractice(quiz.id);
      return;
    }

    // Nếu là chủ bộ hoặc admin -> đi luyện luôn (không cần mở khóa)
    const roles = this.tokenService.getRoles();
    const isAdmin = roles.includes('ROLE_ADMIN');
    if (quiz.nguoi_tao_id === this.currentUserId || isAdmin) {
      this.navigatePractice(quiz.id);
      return;
    }

    const price = quiz.gia_mo_khoa ?? 0;

    Swal.fire({
      icon: 'question',
      title: 'Mở khoá bộ câu hỏi?',
      html: `
        <p>Bộ: <strong>${quiz.tieu_de}</strong></p>
        <p>Giá mở khoá: <strong>${price} vàng</strong></p>
      `,
      showCancelButton: true,
      confirmButtonText: 'Mở khoá',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (result.isConfirmed) {
        this.doUnlockBoCauHoi(quiz);
      }
    });
  }

  /** Gọi API mở khoá bộ câu hỏi */
  private doUnlockBoCauHoi(quiz: BoCauHoiResponse) {
    this.unlocking_id = quiz.id;

    this.bocauHoiService.unlock_bo_cau_hoi(quiz.id).subscribe({
      next: (res: ResponseObject<any>) => {
        this.unlocking_id = null;
        const data = res.data;

        Swal.fire({
          icon: 'success',
          title: data?.da_mo_khoa_truoc_do ? 'Đã mở khoá từ trước' : 'Mở khoá thành công!',
          html: `
            <p>Bộ: <strong>${quiz.tieu_de}</strong></p>
            <p>Đã trừ: <strong>${
              data?.da_mo_khoa_truoc_do ? 0 : data?.gia_mo_khoa
            } vàng</strong></p>
            <p>Vàng còn lại: <strong>${data?.tien_vang_sau}</strong></p>
          `,
          confirmButtonText: 'Luyện ngay',
        }).then(() => {
          // Reload lại danh sách để cập nhật trạng thái unlock (button sẽ chuyển từ "Mở khoá" sang "Luyện")
          this.loadData();
          // Sau đó đi luyện
          this.navigatePractice(quiz.id);
        });
      },
      error: (err) => {
        this.unlocking_id = null;
        const msg = err?.error?.message || 'Không thể mở khoá bộ câu hỏi';
        Swal.fire('Lỗi', msg, 'error').then((r) => {});
      },
    });
  }

  /** Điều hướng sang trang luyện tập bộ câu hỏi */
  private navigatePractice(id: number) {
    this.router
      .navigate(['/luyen-tap'], {
        queryParams: {
          bo_cau_hoi_id: id,
        },
      })
      .then((r) => {});
  }
}
