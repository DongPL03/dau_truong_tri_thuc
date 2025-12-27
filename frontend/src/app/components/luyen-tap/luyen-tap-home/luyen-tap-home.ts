import {CommonModule} from '@angular/common';
import {Component, computed, OnDestroy, OnInit, signal, ViewChild} from '@angular/core';
import {FormsModule, NgForm} from '@angular/forms';
import {environment} from 'src/app/environments/environment';
import Swal from 'sweetalert2';
import {BatDauLuyenTapRequest} from '../../../dtos/luyen-tap/bat_dau_luyen_tap-request';
import {CauTraLoiPracticeDTO, TraLoiCauHoiPracticeDTO,} from '../../../dtos/luyen-tap/tra-loi-cau-hoi-dto';
import {BoCauHoiResponse} from '../../../responses/bocauhoi/bocauhoi-response';
import {KhoaHoiResponse} from '../../../responses/khoahoc/khoa-hoi-response';
import {BatDauLuyenTapResponse, CauHoiPracticeItem,} from '../../../responses/luyentap/bat_dau_luyen_tap-response';
import {LichSuLuyenTapItem} from '../../../responses/luyentap/lich_su_luyen_tap-item';
import {SubmitLuyenTapResponse} from '../../../responses/luyentap/submit_luyen_tap-response';
import {TheGhiNhoResponse} from '../../../responses/luyentap/the_ghi_nho-response';
import {PageResponse} from '../../../responses/page-response';
import {ResponseObject} from '../../../responses/response-object';
import {Base} from '../../base/base';

interface LocalAnswerState {
  lua_chon?: 'A' | 'B' | 'C' | 'D' | null;
  start_time_ms: number; // thời điểm bắt đầu câu
  elapsed_ms?: number; // thời gian đã trả lời (ms)
}

@Component({
  selector: 'app-luyen-tap-home',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './luyen-tap-home.html',
  styleUrl: './luyen-tap-home.scss',
})
export class LuyenTapHomeComponent extends Base implements OnInit, OnDestroy {
  @ViewChild('f') f!: NgForm;

  dto: BatDauLuyenTapRequest = new BatDauLuyenTapRequest();

  // state
  loading = signal<boolean>(false);
  playing = signal<boolean>(false);
  submitting = signal<boolean>(false);
  finished = signal<boolean>(false);

  practice_data = signal<BatDauLuyenTapResponse | null>(null);
  current_index = signal<number>(0);

  // lưu đáp án & thời gian cho từng câu
  private answers_map = new Map<number, LocalAnswerState>();
  private answers_version = signal<number>(0);

  // kết quả sau submit
  result = signal<SubmitLuyenTapResponse | null>(null);

  seconds_per_question = 15;

  remaining_seconds = signal<number>(0);
  private questionTimer?: ReturnType<typeof setInterval>;

  bo_cau_hoi_id: number | null = null;
  // không dùng nữa, cho về 0

  // danh sách bộ câu hỏi
  bo_cau_hoi_options = signal<BoCauHoiResponse[]>([]);
  loading_sets = signal<boolean>(false);

  practice_from_memo = signal<boolean>(false);

  // 🆕 lịch sử luyện tập
  history_items = signal<LichSuLuyenTapItem[]>([]);
  history_page = signal<number>(0);
  history_total_pages = signal<number>(0);
  loading_history = signal<boolean>(false);
  totalPagesHistory = 0;
  pageHistory = 0;
  filterKhoaHocId: number | null = null;
  filterBoCauHoiId: number | null = null;
  khoa_hoc_options = signal<KhoaHoiResponse[]>([]);

  // 🆕 Thẻ ghi nhớ
  memo_items = signal<TheGhiNhoResponse[]>([]);
  memo_page = signal<number>(0);
  memo_total_pages = signal<number>(0);
  loading_memos = signal<boolean>(false);
  totalPagesMemo = 0;
  pageMemo = 0;

  protected readonly environment = environment;

  ngOnInit(): void {
    this.loadPracticeSets();
    this.loadKhoaHocOptions();

    this.route.queryParamMap.subscribe((params) => {
      const idParam = params.get('bo_cau_hoi_id');
      const parsed = idParam ? Number(idParam) : NaN;

      if (!isNaN(parsed) && parsed > 0) {
        this.bo_cau_hoi_id = parsed;
        this.onStart();
      }
    });

    this.loadHistory(0);
    this.loadMemos(0);
  }

  onStart(form?: NgForm) {
    if (!this.bo_cau_hoi_id) {
      Swal.fire('Thiếu thông tin', 'Vui lòng chọn bộ câu hỏi', 'info').then((r) => {
      });
      return;
    }

    this.loading.set(true);
    this.practice_from_memo.set(false);
    this.dto = {
      bo_cau_hoi_id: this.bo_cau_hoi_id,
      so_luong: 0, // 0 = lấy hết câu trong bộ
    };

    this.luyenTapService.startPractice(this.dto).subscribe({
      next: (res: ResponseObject<BatDauLuyenTapResponse>) => {
        this.loading.set(false);
        const data = res.data!;
        if (!data || !data.cau_hoi_list || data.cau_hoi_list.length === 0) {
          Swal.fire('Không có câu hỏi', 'Bộ câu hỏi này chưa có câu hỏi', 'info').then((r) => {
          });
          return;
        }

        this.practice_data.set(data);
        this.current_index.set(0);
        this.answers_map.clear();
        this.answers_version.update((v) => v + 1);
        this.result.set(null);
        this.playing.set(true);
        this.finished.set(false);

        // bắt đầu từ câu 0
        this.startQuestion(0);
        this.setupPracticeSession(data);
      },
      error: (err) => {
        this.loading.set(false);
        Swal.fire('Lỗi', err?.error?.message || 'Không thể bắt đầu luyện tập', 'error').then(
          (r) => {
          }
        );
      },
    });
  }

  onStartFromMemo() {
    if (!this.bo_cau_hoi_id) {
      Swal.fire('Thiếu thông tin', 'Vui lòng chọn bộ câu hỏi trước', 'info').then((r) => {
      });
      return;
    }

    this.loading.set(true);
    this.practice_from_memo.set(true);

    this.luyenTapService.startPracticeFromMemos(this.bo_cau_hoi_id).subscribe({
      next: (res: ResponseObject<BatDauLuyenTapResponse>) => {
        this.loading.set(false);
        const data = res.data!;
        if (!data || !data.cau_hoi_list || data.cau_hoi_list.length === 0) {
          Swal.fire(
            'Không có thẻ ghi nhớ',
            'Bạn chưa có thẻ ghi nhớ nào cho bộ câu hỏi này',
            'info'
          ).then((r) => {
          });
          this.practice_from_memo.set(false);
          return;
        }

        this.practice_data.set(data);
        this.current_index.set(0);
        this.answers_map.clear();
        this.answers_version.update((v) => v + 1);
        this.result.set(null);
        this.playing.set(true);
        this.finished.set(false);

        this.startQuestion(0);
        this.setupPracticeSession(data);
      },
      error: (err) => {
        this.loading.set(false);
        this.practice_from_memo.set(false);
        Swal.fire(
          'Lỗi',
          err?.error?.message || 'Không thể bắt đầu luyện tập từ thẻ ghi nhớ',
          'error'
        ).then((r) => {
        });
      },
    });
  }

  // ================== SETUP VERSION MỚI ==================
  private setupPracticeSession(data: BatDauLuyenTapResponse) {
    this.practice_data.set(data);
    this.current_index.set(0);
    this.answers_map.clear();
    this.answers_version.update((v) => v + 1);
    this.result.set(null);
    this.playing.set(true);
    this.finished.set(false);

    this.startQuestion(0);

    // init start_time cho câu đầu
    const firstQ = data.cau_hoi_list[0];
    this.answers_map.set(firstQ.id, {
      lua_chon: null,
      start_time_ms: Date.now(),
    });
  }

  ngOnDestroy() {
    this.clearTimer();
  }

  private loadPracticeSets() {
    this.loading_sets.set(true);
    this.bocauHoiService.getPracticeSets().subscribe({
      next: (res: ResponseObject<PageResponse<BoCauHoiResponse>>) => {
        this.loading_sets.set(false);
        const page = res.data!;
        const list = page?.items ?? []; // nếu PageResponse dùng 'items'
        this.bo_cau_hoi_options.set(list);
      },
      error: (err) => {
        this.loading_sets.set(false);
        Swal.fire(
          'Lỗi',
          err?.error?.message || 'Không lấy được danh sách bộ câu hỏi',
          'error'
        ).then((r) => {
        });
      },
    });
  }

  /** Load danh sách khóa học để filter lịch sử */
  private loadKhoaHocOptions() {
    // Lấy tối đa 50 khóa học published để filter
    this.khoaHocService.getAll('', 0, 'PUBLISHED', 'NEWEST', 0, 50).subscribe({
      next: (res: ResponseObject<PageResponse<KhoaHoiResponse>>) => {
        const page = res.data!;
        this.khoa_hoc_options.set(page?.items ?? []);
      },
      error: (err) => {
        console.error('Không lấy được danh sách khóa học để filter lịch sử', err);
      },
    });
  }

  // 🆕 hàm load history
  loadHistory(page: number) {
    this.loading_history.set(true);
    const size = 5;

    this.luyenTapService
      .getHistory(page, size, this.filterKhoaHocId || undefined, this.filterBoCauHoiId || undefined)
      .subscribe({
        next: (res: ResponseObject<PageResponse<LichSuLuyenTapItem>>) => {
          this.loading_history.set(false);
          const pageData = res.data!;
          this.history_items.set(pageData?.items ?? []);
          this.history_page.set(pageData?.currentPage ?? 0);
          this.totalPagesHistory = pageData.totalPages;
          this.history_total_pages.set(pageData?.totalPages ?? 0);
        },
        error: (err) => {
          this.loading_history.set(false);
          Swal.fire('Lỗi', err?.error?.message || 'Không lấy được lịch sử luyện tập', 'error').then(
            (r) => {
            }
          );
        },
      });
  }

  // 🆕 load danh sách thẻ ghi nhớ
  loadMemos(page: number) {
    this.loading_memos.set(true);
    const size = 5;

    this.luyenTapService.getMemos(page, size).subscribe({
      next: (res: ResponseObject<PageResponse<TheGhiNhoResponse>>) => {
        this.loading_memos.set(false);
        const pageData = res.data!;
        this.memo_items.set(pageData?.items ?? []);
        this.memo_page.set(pageData?.currentPage ?? 0);
        this.totalPagesMemo = pageData.totalPages;
        this.memo_total_pages.set(pageData?.totalPages ?? 0);
      },
      error: (err) => {
        this.loading_memos.set(false);
        Swal.fire(
          'Lỗi',
          err?.error?.message || 'Không lấy được danh sách thẻ ghi nhớ',
          'error'
        ).then((r) => {
        });
      },
    });
  }

  // 🆕 điều hướng trang trước / sau
  prevHistoryPage() {
    const p = this.history_page();
    if (p > 0) {
      this.loadHistory(p - 1);
    }
  }

  nextHistoryPage() {
    const p = this.history_page();
    const total = this.history_total_pages();
    if (p < total - 1) {
      this.loadHistory(p + 1);
    }
  }

  getVisiblePages(): number[] {
    const visible: number[] = [];
    const maxVisible = 4; // số nút trang hiển thị tối đa
    const total = this.totalPagesHistory;

    if (total <= maxVisible) {
      return Array.from({length: total}, (_, i) => i);
    }

    const start = Math.max(0, this.pageHistory - 3);
    const end = Math.min(total - 1, this.pageHistory + 3);

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
    if (p < 0 || p >= this.totalPagesHistory) return;
    this.pageHistory = p;
    this.loadHistory(p);
  }

  getVisiblePagesMemo(): number[] {
    const visible: number[] = [];
    const maxVisible = 4; // số nút trang hiển thị tối đa
    const total = this.totalPagesMemo;

    if (total <= maxVisible) {
      return Array.from({length: total}, (_, i) => i);
    }

    const start = Math.max(0, this.pageMemo - 3);
    const end = Math.min(total - 1, this.pageMemo + 3);

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

  changePageMemo(p: number) {
    if (p < 0 || p >= this.totalPagesMemo) return;
    this.pageMemo = p;
    this.loadMemos(p);
  }

  currentQuestion = computed<CauHoiPracticeItem | null>(() => {
    const data = this.practice_data();
    const idx = this.current_index();
    if (!data || !data.cau_hoi_list || idx < 0 || idx >= data.cau_hoi_list.length) {
      return null;
    }
    return data.cau_hoi_list[idx];
  });

  currentAnswer = computed<'A' | 'B' | 'C' | 'D' | ''>(() => {
    const q = this.currentQuestion();

    // 👇 dòng này chỉ để tạo dependency, không dùng giá trị
    this.answers_version();

    if (!q) return '';
    const st = this.answers_map.get(q.id);
    return st?.lua_chon ?? '';
  });

  private startQuestion(index: number) {
    const data = this.practice_data();
    if (!data || index < 0 || index >= data.cau_hoi_list.length) return;

    // set index hiện tại
    this.current_index.set(index);

    const q = data.cau_hoi_list[index];
    const now = Date.now();

    const existed = this.answers_map.get(q.id);
    if (!existed) {
      this.answers_map.set(q.id, {
        lua_chon: null,
        start_time_ms: now,
      });
    } else {
      existed.start_time_ms = now;
      this.answers_map.set(q.id, existed);
    }

    this.startTimer();
  }

  private startTimer() {
    this.clearTimer();
    this.remaining_seconds.set(this.seconds_per_question);

    this.questionTimer = setInterval(() => {
      const r = this.remaining_seconds() - 1;
      this.remaining_seconds.set(r);

      if (r <= 0) {
        this.handleTimeout();
      }
    }, 1000);
  }

  private saveElapsedForCurrent() {
    const q = this.currentQuestion();
    if (!q) return;

    const st = this.answers_map.get(q.id);
    if (!st) return;

    // đơn giản: thời gian đã dùng = (seconds_per_question - remaining_seconds) * 1000
    const used = (this.seconds_per_question - this.remaining_seconds()) * 1000;
    st.elapsed_ms = used > 0 ? used : 0;
    this.answers_map.set(q.id, st);
  }

  private clearTimer() {
    if (this.questionTimer) {
      clearInterval(this.questionTimer);
      this.questionTimer = undefined;
    }
  }

  submitCurrentQuestion() {
    const q = this.currentQuestion();
    if (!q) return;

    const answer = this.currentAnswer();
    if (!answer) {
      Swal.fire('Chưa chọn đáp án', 'Hãy chọn A/B/C/D trước khi nộp', 'info').then((r) => {
      });
      return;
    }

    // lưu thời gian
    this.saveElapsedForCurrent();

    // chuyển câu hoặc submit toàn bài
    this.goNextOrFinish();
  }

  private handleTimeout() {
    // hết giờ: lưu thời lượng, có thể không có lua_chon (coi như bỏ qua)
    this.saveElapsedForCurrent();
    this.goNextOrFinish();
  }

  private goNextOrFinish() {
    const data = this.practice_data();
    if (!data) return;

    this.clearTimer();

    const idx = this.current_index();
    const isLast = idx >= data.cau_hoi_list.length - 1;

    if (!isLast) {
      // sang câu tiếp theo
      this.startQuestion(idx + 1);
    } else {
      // đã tới câu cuối cùng → gửi kết quả toàn phiên
      this.onSubmitPractice();
    }
  }

  selectAnswer(opt: 'A' | 'B' | 'C' | 'D') {
    const q = this.currentQuestion();
    if (!q) return;

    const existed = this.answers_map.get(q.id);
    if (!existed) {
      this.answers_map.set(q.id, {
        lua_chon: opt,
        start_time_ms: Date.now(),
      });
    } else {
      existed.lua_chon = opt;
      this.answers_map.set(q.id, existed);
    }

    // 👇 báo cho computed biết state đã đổi
    this.answers_version.update((v) => v + 1);
  }

  goPrev() {
    const data = this.practice_data();
    const idx = this.current_index();
    if (!data || idx <= 0) return;
    this.clearTimer();
    this.startQuestion(idx - 1);
  }

  goNext() {
    const data = this.practice_data();
    const idx = this.current_index();
    if (!data || idx >= data.cau_hoi_list.length - 1) return;
    this.clearTimer();
    this.startQuestion(idx + 1);
  }

  onSubmitPractice() {
    const data = this.practice_data();
    if (!data) return;

    const phien_id = data.phien_id;

    const cau_tra_loi_list: CauTraLoiPracticeDTO[] = data.cau_hoi_list.map((q) => {
      const st = this.answers_map.get(q.id);
      return {
        cau_hoi_id: q.id,
        lua_chon: st?.lua_chon ?? null, // có thể null nếu hết giờ chưa chọn
        thoi_gian_ms: st?.elapsed_ms ?? null, // thời gian trả lời (ms)
      };
    });

    const dto: TraLoiCauHoiPracticeDTO = {
      phien_id,
      cau_tra_loi_list,
    };

    this.submitting.set(true);
    this.luyenTapService.submitPractice(dto).subscribe({
      next: (res: ResponseObject<SubmitLuyenTapResponse>) => {
        this.submitting.set(false);
        this.result.set(res.data!);
        this.finished.set(true);
        this.playing.set(false);
        Swal.fire('Hoàn thành', 'Bạn đã nộp bài luyện tập', 'success').then((r) => {
        });
      },
      error: (err) => {
        this.submitting.set(false);
        Swal.fire('Lỗi', err?.error?.message || 'Không thể nộp bài luyện tập', 'error').then(
          (r) => {
          }
        );
      },
    });
  }

  deleteMemoConfirm(memo: TheGhiNhoResponse) {
    Swal.fire({
      title: 'Xoá thẻ ghi nhớ?',
      text: `Câu hỏi: "${memo.cau_hoi}"`,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xoá',
      cancelButtonText: 'Huỷ',
    }).then((result) => {
      if (result.isConfirmed) {
        this.luyenTapService.deleteMemo(memo.memo_id).subscribe({
          next: () => {
            Swal.fire('Đã xoá', 'Đã xoá thẻ ghi nhớ.', 'success').then((r) => {
            });
            // load lại current page
            this.loadMemos(this.memo_page());
          },
          error: (err) => {
            Swal.fire('Lỗi', err?.error?.message || 'Không xoá được thẻ ghi nhớ', 'error').then(
              (r) => {
              }
            );
          },
        });
      }
    });
  }

  // 🔁 Luyện đúng 1 câu từ thẻ ghi nhớ
  practiceSingleFromMemo(memo: TheGhiNhoResponse) {
    // 1. Tìm bộ câu hỏi tương ứng trong danh sách practice_sets
    const list = this.bo_cau_hoi_options();
    const found = list.find((b) => b.tieu_de === memo.bo_cau_hoi);

    if (!found) {
      Swal.fire(
        'Không tìm thấy bộ câu hỏi',
        'Bộ câu hỏi của thẻ ghi nhớ này không còn khả dụng để luyện tập.',
        'info'
      ).then((r) => {
      });
      return;
    }

    this.bo_cau_hoi_id = found.id;
    this.loading.set(true);
    this.practice_from_memo.set(true);

    // 2. Gọi API luyện tập từ thẻ ghi nhớ của cả bộ,
    //    sau đó FILTER lại chỉ còn đúng 1 câu của memo
    this.luyenTapService.startPracticeFromMemos(this.bo_cau_hoi_id).subscribe({
      next: (res: ResponseObject<BatDauLuyenTapResponse>) => {
        this.loading.set(false);
        const data = res.data!;
        if (!data || !data.cau_hoi_list || data.cau_hoi_list.length === 0) {
          Swal.fire('Không có câu hỏi', 'Không tìm thấy câu hỏi để luyện tập.', 'info').then(
            (r) => {
            }
          );
          this.practice_from_memo.set(false);
          return;
        }

        // 3. Lọc theo nội dung câu hỏi (memo.cau_hoi)
        const filtered = data.cau_hoi_list.filter((q) => q.noi_dung === memo.cau_hoi);

        if (!filtered.length) {
          Swal.fire(
            'Không tìm thấy câu hỏi',
            'Câu hỏi trong thẻ ghi nhớ không còn tồn tại trong bộ.',
            'info'
          ).then((r) => {
          });
          this.practice_from_memo.set(false);
          return;
        }

        // 4. Tạo data mới chỉ còn đúng các câu đã lọc (thường là 1 câu)
        const filteredData: BatDauLuyenTapResponse = {
          ...data,
          tong_cau_hoi: filtered.length,
          cau_hoi_list: filtered,
        };

        // 5. Khởi tạo phiên luyện tập như bình thường
        this.setupPracticeSession(filteredData);
      },
      error: (err) => {
        this.loading.set(false);
        this.practice_from_memo.set(false);
        Swal.fire(
          'Lỗi',
          err?.error?.message || 'Không thể bắt đầu luyện tập từ thẻ ghi nhớ',
          'error'
        ).then((r) => {
        });
      },
    });
  }

  // 🔁 Luyện lại từ tất cả thẻ ghi nhớ của bộ này (reuse onStartFromMemo)
  practiceSetFromMemo(memo: TheGhiNhoResponse) {
    const list = this.bo_cau_hoi_options();
    const found = list.find((b) => b.tieu_de === memo.bo_cau_hoi);

    if (!found) {
      Swal.fire(
        'Không tìm thấy bộ câu hỏi',
        'Bộ câu hỏi của thẻ ghi nhớ này không còn khả dụng để luyện tập.',
        'info'
      ).then((r) => {
      });
      return;
    }

    this.bo_cau_hoi_id = found.id;
    // dùng lại logic đã có: luyện tập từ thẻ ghi nhớ theo bộ
    this.onStartFromMemo();
  }

  // Sau này dùng cho nút "Luyện tập lại"
  restartWithSameSet() {
    const data = this.practice_data();
    if (!data) return;
    this.bo_cau_hoi_id = this.bo_cau_hoi_id ?? null;
    this.onStart({} as NgForm);
  }

  retryHistory(h: any) {
    if (!h.bo_cau_hoi_id) {
      Swal.fire('Lỗi', 'Không tìm thấy thông tin bộ câu hỏi này', 'error');
      return;
    }
    this.bo_cau_hoi_id = h.bo_cau_hoi_id;
    window.scrollTo({ top: 0, behavior: 'smooth' });
    this.onStart();
  }

  getAccuracyClass(acc: number): string {
    if (acc >= 80) return 'high'; // Xanh
    if (acc >= 50) return 'med';  // Vàng
    return 'low';                 // Đỏ
  }
}
